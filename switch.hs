import Text.Read (readMaybe)
import Data.IntMap.Strict (toList, fromList, delete)
import Network
import System.IO (Handle, hGetLine, hPutStrLn, hPutStr, hClose)
import System.IO.Error (isEOFError)
import System.Timeout (timeout)
import Control.Exception.Base (try)
import Control.Concurrent (forkIO)
import Control.Monad (when)
import Control.Monad.STM (atomically, orElse, STM)
import Control.Applicative
import Control.Concurrent.STM.TChan (TChan, newTChan, dupTChan, readTChan, tryReadTChan, writeTChan)

import Switch.Message (checkFormat, readDestination, readSource, readText, header)
import Switch.Address (readRequest, respondAccepted, respondNotAccepted)
import Switch.Types
import Switch.TimeStamp (stringStamp)
import qualified Switch.Tags as Tg

{-
 Initial sequence
-}

main :: IO()
main = do
  controlChannel <- atomically newTChan
  readChannel <- atomically newTChan
  let channels = (controlChannel, readChannel)
  socket <- listenOn $ PortNumber 4242
  putStrLn $ Tg.control ++ "Starting switch on port: " ++ show 4242
  forkIO $ switch channels []
  acceptLoop socket channels

{-
 Accept loop with function for obtaining address
-}

acceptLoop socket channels = do
  (handle,_,_) <- accept socket
  let controlChannel = fst channels
      readChannel    = snd channels
  addr <- obtainAddress handle
  case addr of
    Just address -> do
        atomically $ writeTChan controlChannel (RequestAddr,address,handle)
        acceptLoop socket channels
    Nothing -> do
        acceptLoop socket channels

-- Read two lines from user, decide whether it is a request
-- for address and do sth about it
obtainAddress handle = do
  msg <- sequence $ take 2 $ repeat $ hGetLine handle
  let mAddr = readRequest $ unlines msg
  case mAddr of
    Just addr -> do -- send control message do switch thread
      putStrLn $ Tg.control ++ "Obtained address request from client: " ++ show addr
      return $ Just addr
    Nothing -> do -- say bye, bye to client and close handle
      hPutStrLn handle "Wrong initial sequence! Switch expected address of your client."
      hPutStrLn handle "Bye!"
      hClose handle
      return Nothing

{-
 Main switch loop and its functions
-}

switch channels clients = do
  let (controlChannel,readChannel) = channels
  input <- atomically $ select readChannel controlChannel
  case input of
    -- Message came to switch
    Left message -> do
        let maybeChannel = lookup (readDestination message) clients
        case maybeChannel of
          Nothing -> mapM (\(_,channel) -> atomically (writeTChan channel message)) clients
          Just chan -> mapM (\channel -> atomically (writeTChan channel message)) [chan]
        switch channels clients
    -- Control message came to switch
    Right clientTuple -> do
        let (msgType,addr,handle) = clientTuple
        if msgType == RequestAddr
           then do
               let maybeChannel = lookup addr clients
               case maybeChannel of
                 Nothing -> do
                   newClientChannel <- forkNewClient controlChannel readChannel handle addr
                   let newClientTuple = (addr,newClientChannel)
                   putStr $ Tg.control ++ "Added new client with address: " ++ show addr ++ ". Clients:"
                   dbgShowClients (newClientTuple:clients)
                   switch channels (newClientTuple:clients)
                 Just _ -> do
                   putStrLn $ Tg.control ++ "Attempt to use duplicate address. Connection closed."
                   hPutStrLn handle "Address is already in use. Pick some other number."
                   hPutStrLn handle "Bye!"
                   hClose handle
                   switch channels clients
           else do
               let maybeChannel = lookup addr clients
               case maybeChannel of
                 Nothing -> do
                     putStrLn $ Tg.err ++ "Internal error occured. Attempt to delete non-existing address"
                     switch channels clients
                 Just chan -> do
                     let newClients = addr `removeFrom` clients
                     putStr $ Tg.control ++ "Client " ++ show addr ++ " removed. Clients: "
                     dbgShowClients newClients
                     hClose handle
                     switch channels newClients
                     where removeFrom a = toList.(delete a).fromList

-- Read from two channels (user, control) "simultaneousely" and return messages
select :: TChan a -> TChan b -> STM (Either a b)
select ch1 ch2 = do
  a <- readTChan ch1; return (Left a)
  `orElse` do
  b <- readTChan ch2; return (Right b)

dbgShowClients [] = putStr "\n"

dbgShowClients (x:xs)= do
    let (addr,_) = x
    putStr $ "("++show addr++"),"
    dbgShowClients xs

forkNewClient controlChannel readChannel handle addr = do
  --putStrLn $ "Creating new client with address: " ++ show address
  clientReadChannel <- atomically $ dupTChan readChannel
  clientWriteChannel <- atomically newTChan
  killSignal <- atomically newTChan
  forkIO $ readFromClient addr handle controlChannel clientReadChannel killSignal
  forkIO $ writeToClient addr handle clientWriteChannel killSignal
  return clientWriteChannel

{-
 Client thread functions
-}
readFromClient :: Address -> Handle -> TChan (ControlMsg,Address,Handle) -> TChan Message -> TChan [Char] -> IO ()
readFromClient addr handle controlChannel channel killSignal = do
  maybeInput <- timeout (15*10^6) $ readMessage handle
  case maybeInput of
    Nothing -> do
      putStrLn $ Tg.control ++ "Timeout for client " ++ show addr
      hPutStrLn handle "Timeout. Bye!"
      killClient
    Just maybeMsg ->
      case maybeMsg of
        Nothing -> do
          putStrLn $ Tg.control ++ "Reached EOF of client " ++ show addr
          killClient
        Just message -> do
          if checkFormat message == False
            then hPutStrLn handle "Error! Bad message format."
            else if readSource message /= addr
                    then hPutStrLn handle "Wrong source address!"
                    else do
                        let src = show . readSource $ message
                            dest = show . readDestination $ message
                            txt = readText message
                        atomically $ writeTChan channel message
                        timeStamp <- stringStamp
                        let msgDump = src++ "->" ++dest++ ": " ++txt
                            timeTag = " " ++ timeStamp ++ " "
                        putStrLn $ Tg.client ++ timeTag ++ msgDump
          readFromClient addr handle controlChannel channel killSignal

  where killClient = do
          atomically $ writeTChan killSignal "kill"
          atomically $ writeTChan controlChannel (DeleteAddr,addr,handle)

readMessage :: Handle -> IO(Maybe Message)
readMessage handle = do
  msgheader <- maybeReadLine handle
  case msgheader of
    Just line -> if line == header
                    then do
                      msglines <- sequence $ take 3 $ repeat $ maybeReadLine handle
                      let addNewLines = map (\x-> fmap (++ "\n") x)
                          maybeFold = foldl (\acc x -> (++) <$> acc <*> x) (Just [])
                          maybeUnlines = maybeFold . addNewLines
                      return $ maybeUnlines (Just header:msglines)
                    else readMessage handle
    Nothing -> return Nothing

maybeReadLine :: Handle -> IO(Maybe String)
maybeReadLine handle = do
  input <- try $ hGetLine handle
  case input of
    Left e -> do
      if isEOFError e
         then do
             return Nothing
         else ioError e
    Right line -> return $ Just line

writeToClient :: Address -> Handle -> TChan Message -> TChan [Char] -> IO ()
writeToClient addr handle channel killSignal = do
  input <- atomically $ select channel killSignal
  case input of
    Left message -> do
        when (readSource message /= addr) $ hPutStrLn handle message
        writeToClient addr handle channel killSignal
    Right signal -> do
        if signal == "kill"
           then putStrLn $ Tg.control ++ "Client write thread killed"
           else putStrLn $ Tg.err ++ "Client write thread obtained unknown signal"
