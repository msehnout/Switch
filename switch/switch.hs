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

import Switch.Message (checkFormat, readDestination, readSource, readText, header, readMessage)
import Switch.Address (readRequest, respondAccepted, respondNotAccepted)
import Switch.Types
import Switch.TimeStamp (stringStamp)
import qualified Switch.Tags as Tg

{-
 Initial sequence
-}

main :: IO()
main = do
  controlChannel <- atomically newTChan     --channel for control signals between threads
  readChannel <- atomically newTChan        --common channel for reading from all sockets
  let channels = (controlChannel, readChannel)
  socket <- listenOn $ PortNumber 4242      --listen on localhost:4242
  putStrLn $ Tg.control ++ "Starting switch on port: " ++ show 4242
  --fork new thread performing switch functionality with channels and empty list of users
  forkIO $ switch channels []
  --enter loop for accepting new users
  acceptLoop socket channels

{-
 Accept loop with function for obtaining address
-}
acceptLoop socket channels = do
  --wait for new connection
  (handle,hostname,port) <- accept socket
  --show debug output
  putStrLn $ Tg.control ++ "New user connected from: " ++ hostname ++ ":" ++ show port
  let controlChannel = fst channels
      readChannel    = snd channels
  --fork new thread for obtaining address from user
  forkIO $ obtainAddress controlChannel handle
  acceptLoop socket channels

  where
  -- Read two lines from user, decide whether it is a request
  -- for address and do sth about it
  obtainAddress controlChannel handle = do
    addr <- obtainAddressWait handle
    --If address was successfully obtained, send message to switch thread
    --through control channel
    when (addr /= Nothing ) $ do
      let Just address = addr
      atomically $ writeTChan controlChannel (RequestAddr,address,handle)
  --Get two lines from user. Function is wrapped into EOF check and timeout check.
  --Return maybe address.
  obtainAddressWait handle = do
    maybeMsg <- timeout (15*10^6) $ sequence $ take 2 $ repeat $ hGetLine handle
    case maybeMsg of
      Just msg -> do
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
      Nothing -> do
        hPutStrLn handle "Timeout exceeded."
        hPutStrLn handle "Bye!"
        hClose handle
        return Nothing

{-
 Main switch loop and its functions
-}

switch channels clients = do
  let (controlChannel,readChannel) = channels
  --Waiting for messages from user threads or control channel
  input <- atomically $ select readChannel controlChannel
  case input of
    -- Message from user came to switch
    Left message -> do
        --Obtain time stamp for debug output
        timeStamp <- stringStamp
        --Look for user in users list
        --users list is list of pairs: [(address,channel to write to this client)]
        let maybeChannel = lookup (readDestination message) clients
            --Create debug output from message
            src = show . readSource $ message
            dest = show . readDestination $ message
            txt = readText message
            msgDump = src++ "->" ++dest++ ": " ++txt
            timeTag = " " ++ timeStamp ++ " "
            printMsg msgtype = putStrLn $ Tg.client ++ timeTag ++ msgtype ++ msgDump
        case maybeChannel of
          --The client with this address was not found. Broadcast this message.
          Nothing -> do
            printMsg "(Broadcast) "
            mapM (\(_,channel) -> atomically (writeTChan channel message)) clients
          --The client was found. Send message directly to him.
          Just chan -> do
            printMsg "(Unicast) "
            mapM (\channel -> atomically (writeTChan channel message)) [chan]
        switch channels clients

    -- Control message came to switch
    Right clientTuple -> do
        let (msgType,addr,handle) = clientTuple
        -- New user request address
        if msgType == RequestAddr
           then do
               --Check whether client with this address already exist.
               let maybeChannel = lookup addr clients
               case maybeChannel of
                 --Accept new client.
                 Nothing -> do
                   newClientChannel <- forkNewClient controlChannel readChannel handle addr
                   let newClientTuple = (addr,newClientChannel)
                   putStr $ Tg.control ++ "Added new client with address: " ++ show addr ++ ". Clients:"
                   dbgShowClients (newClientTuple:clients)
                   switch channels (newClientTuple:clients)
                 --Don't accept new client.
                 Just _ -> do
                   putStrLn $ Tg.control ++ "Attempt to use duplicate address. Connection closed."
                   hPutStrLn handle "Address is already in use. Pick some other number."
                   hPutStrLn handle "Bye!"
                   hClose handle
                   switch channels clients
           -- Timeout of some user exceeded or EOF reached. The user is going to be removed.
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
         -- Print connected users
   where dbgShowClients [] = putStr "\n"
         dbgShowClients (x:xs)= do
            let (addr,_) = x
            putStr $ "("++show addr++"),"
            dbgShowClients xs

          -- New user connected and his request was accepted => read/write threads are forked
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
readFromClient addr handle controlChannel channel killSignal = do
  maybeInput <- timeout (15*10^6) $ readMessage handle
  case maybeInput of
    -- Check if there was a timeout
    Nothing -> do
      putStrLn $ Tg.control ++ "Timeout for client " ++ show addr
      hPutStrLn handle "Timeout. Bye!"
      killClient
    Just maybeMsg ->
      -- Check if input is EOF
      case maybeMsg of
        Nothing -> do
          putStrLn $ Tg.control ++ "Reached EOF of client " ++ show addr
          killClient
        -- Everything went fine => message obtained
        Just message -> do
          if checkFormat message == False
            then hPutStrLn handle "Error! Bad message format."
            else if readSource message /= addr
                    then hPutStrLn handle "Wrong source address!"
                    else do
                        -- If message format and source address are ok, send it to switch
                        atomically $ writeTChan channel message
          readFromClient addr handle controlChannel channel killSignal

  where killClient = do
          atomically $ writeTChan killSignal "kill"
          atomically $ writeTChan controlChannel (DeleteAddr,addr,handle)


writeToClient addr handle channel killSignal = do
  -- Read messages from switch or read thread (kill signal-used only to end this thread)
  input <- atomically $ select channel killSignal
  case input of
    Left message -> do
        when (readSource message /= addr) $ hPutStrLn handle message
        writeToClient addr handle channel killSignal
    Right signal -> do
        if signal == "kill"
           then do
             putStrLn $ Tg.control ++ "Client write thread killed"
             hClose handle
           else putStrLn $ Tg.err ++ "Client write thread obtained unknown signal"

-- Read from two channels (user, control) "simultaneousely" and return messages
select :: TChan a -> TChan b -> STM (Either a b)
select ch1 ch2 = do
  a <- readTChan ch1; return (Left a)
  `orElse` do
  b <- readTChan ch2; return (Right b)
