import Text.Read (readMaybe)
import Data.IntMap.Strict (toList, fromList, delete)
import Network
import System.IO (hGetLine, hPutStrLn, hPutStr, hClose)
import System.IO.Error (isEOFError)
import System.Timeout (timeout)
import Control.Exception.Base (try)
import Control.Concurrent (forkIO)
import Control.Monad.STM (atomically, orElse, STM)
import Control.Concurrent.STM.TChan (TChan, newTChan, dupTChan, readTChan, tryReadTChan, writeTChan)

import Switch.Message (checkFormat, readDestination, readSource, header)
import Switch.Address (readRequest, respondAccepted, respondNotAccepted)
import Switch.Types

{-
 Initial sequence
-}

main :: IO()
main = do
  controlChannel <- atomically newTChan
  readChannel <- atomically newTChan
  let channels = (controlChannel, readChannel)
  socket <- listenOn $ PortNumber 4242
  putStrLn $ "Starting switch on port: " ++ show 4242
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
      putStrLn $ "Obtained address request from client: " ++ show addr
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
  let controlChannel = fst channels
      readChannel    = snd channels
  input <- atomically $ select readChannel controlChannel
  case input of
    Left message -> do
        let maybeChannel = lookup (readDestination message) clients
        case maybeChannel of
          Nothing -> mapM (\(_,channel) -> atomically (writeTChan channel message)) clients
          Just chan -> mapM (\channel -> atomically (writeTChan channel message)) [chan]
        switch channels clients
    Right clientTuple -> do
        let (msgType,addr,handle) = clientTuple
        if msgType == RequestAddr 
           then do
               let maybeChannel = lookup addr clients
               case maybeChannel of
                 Nothing -> do
                   newClientChannel <- forkNewClient controlChannel readChannel handle addr
                   let newClientTuple = (addr,newClientChannel)
                   putStrLn $ "[internal] Added new client with address: " ++ show addr
                   dbgShowClients (newClientTuple:clients)
                   switch channels (newClientTuple:clients)
                 Just _ -> do
                   putStrLn "[internal] Attempt to use duplicate address. Connection closed."
                   hPutStrLn handle "Address is already in use. Pick some other number."
                   hPutStrLn handle "Bye!"
                   hClose handle
                   switch channels clients
           else do
               let maybeChannel = lookup addr clients
               case maybeChannel of
                 Nothing -> do
                     putStrLn "[Error] Internal error occured. Attempt to delete non-existing address"
                     switch channels clients
                 Just chan -> do
                     let newClients = addr `removeFrom` clients
                     putStrLn $ "[internal] Client " ++ show addr ++ " removed." 
                     hClose handle
                     dbgShowClients newClients 
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
  forkIO $ writeToClient handle clientWriteChannel killSignal
  return clientWriteChannel

{-
 Client thread functions
-}

readFromClient addr handle controlChannel channel killSignal = do
  maybeInput <- timeout (15*10^6) $ try $ hGetLine handle
  case maybeInput of
    Nothing -> do
      putStrLn $ "[internal] Timeout for client " ++ show addr
      hPutStrLn handle "Timeout. Bye!"
      killClient 
    Just input ->
      case input of
        Left e -> do
          if isEOFError e
             then do
                 putStrLn $ "[internal] Reached EOF of client " ++ show addr
                 killClient
             else ioError e
        Right line -> do
          if line == header
            then do
              message <- readMultipleLines handle
              putStr $ "[client channel]\n" ++ message
              let format = checkFormat message
              case format of
                False -> hPutStrLn handle "Error! Bad message format."
                True  -> do
                    if readSource message == addr
                       then atomically $ writeTChan channel message
                       else hPutStrLn handle "Wrong source address!"
            else do
              hPutStrLn handle "Error! Bad message format.2"
          readFromClient addr handle controlChannel channel killSignal

  where killClient = do
          atomically $ writeTChan killSignal "kill"
          atomically $ writeTChan controlChannel (DeleteAddr,addr,handle)

readMultipleLines handle = do
  lines <- sequence $ take 3 $ repeat $ hGetLine handle
  return $ unlines $ header:lines

writeToClient handle channel killSignal = do
  input <- atomically $ select channel killSignal
  case input of
    Left message -> do
        hPutStrLn handle message
        writeToClient handle channel killSignal 
    Right signal -> do
        if signal == "kill"
           then putStrLn "[internal] Client write thread killed"
           else putStrLn "[error] Client write thread obtained unknown signal"
