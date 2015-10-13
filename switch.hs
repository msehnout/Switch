import Text.Read (readMaybe)
import Network
import System.IO (hGetLine, hPutStrLn, hPutStr, hClose)
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
  -- TODO: before accepting new client acqire its address
  addr <- obtainAddress handle
  --putStrLn $ "Creating new client with address: " ++ show address
  --clientReadChannel <- atomically $ dupTChan readChannel
  --clientWriteChannel <- atomically newTChan
  --forkIO $ readFromClient handle clientReadChannel
  --forkIO $ writeToClient handle clientWriteChannel
  case addr of
    Just address -> do
        atomically $ writeTChan controlChannel (RequestAddr,address,handle)--(address,clientWriteChannel)
        acceptLoop socket channels 
    Nothing -> do
        acceptLoop socket channels 

-- Read two lines from user, decide whether it is a request
-- for address and do sth about it
obtainAddress handle = do    
  msg <- sequence $ take 2 $ repeat $ hGetLine handle
  -- let msg = unlines [line1, line2]
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
        let (_,addr,handle) = clientTuple
            maybeChannel = lookup addr clients
        case maybeChannel of
          Nothing -> do
            newClientChannel <- forkNewClient readChannel handle addr
            let newClientTuple = (addr,newClientChannel)
            switch channels (newClientTuple:clients)
          Just _ -> do
            hPutStrLn handle "Address is already in use. Pick some other number."
            hPutStrLn handle "Bye!"
            hClose handle
            switch channels (clients)
 
-- Read from two channels (user, control) "simultaneousely" and return messages
select :: TChan a -> TChan b -> STM (Either a b)
select ch1 ch2 = do 
  a <- readTChan ch1; return (Left a)
  `orElse` do
  b <- readTChan ch2; return (Right b)

forkNewClient readChannel handle addr = do
  --putStrLn $ "Creating new client with address: " ++ show address
  clientReadChannel <- atomically $ dupTChan readChannel
  clientWriteChannel <- atomically newTChan
  forkIO $ readFromClient handle clientReadChannel addr
  forkIO $ writeToClient handle clientWriteChannel
  return clientWriteChannel

{-
 Client thread functions
-}

readFromClient handle channel addr = do
  line <- hGetLine handle
  if line == header
    then do
      message <- readMultipleLines handle
      putStrLn $ "[client channel]\n" ++ message
      let format = checkFormat message
      case format of
        False -> hPutStrLn handle "Error! Bad message format."
        True  -> do
            if readSource message == addr
               then atomically $ writeTChan channel message
               else hPutStrLn handle "Wrong source address!"
    else do
      hPutStrLn handle "Error! Bad message format.2"
  readFromClient handle channel addr

readMultipleLines handle = do
  lines <- sequence $ take 3 $ repeat $ hGetLine handle
  return $ unlines $ header:lines

writeToClient handle channel = do
  message <- atomically $ readTChan channel
  hPutStrLn handle message
  writeToClient handle channel

