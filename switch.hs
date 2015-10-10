import Text.Read (readMaybe)
import Network
import System.IO (hGetLine, hPutStrLn, hPutStr, hClose)
import Control.Concurrent (forkIO)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChan, dupTChan, readTChan, tryReadTChan, writeTChan)

import Switch.Message (checkFormat, readDestination, header)

main :: IO()
main = do
  controlChannel <- atomically newTChan
  readChannel <- atomically newTChan
  let channels = (controlChannel, readChannel)
  socket <- listenOn $ PortNumber 4242
  putStrLn $ "Starting switch on port: " ++ show 4242
  forkIO $ switch channels []
  acceptLoop socket channels 0

acceptLoop socket channels address = do
  (handle,_,_) <- accept socket
  let controlChannel = fst channels
      readChannel    = snd channels
  --hPutStrLn handle "Ahoj ;-) \n Sbohem!"
  --hClose handle
  --acceptLoop socket channels (address+1)
  putStrLn $ "Creating new client with address: " ++ show address
  clientReadChannel <- atomically $ dupTChan readChannel
  clientWriteChannel <- atomically newTChan
  forkIO $ readFromClient handle clientReadChannel
  forkIO $ writeToClient handle clientWriteChannel
  atomically $ writeTChan controlChannel (address,clientWriteChannel)
  acceptLoop socket channels (address+1)

switch channels clients = do
  let controlChannel = fst channels
      readChannel    = snd channels
  message <- atomically $ readTChan readChannel
  newClients <- importNewClients controlChannel clients
  --mapM (\(_,channel) -> atomically (writeTChan channel message)) newClients
  let maybeChannel = lookup (readDestination message) newClients
  case maybeChannel of
    Nothing -> mapM (\(_,channel) -> atomically (writeTChan channel message)) newClients
    Just chan -> mapM (\channel -> atomically (writeTChan channel message)) [chan]
  switch channels newClients

importNewClients controlChannel clients = do
  maybeClient <- atomically $ tryReadTChan controlChannel
  case maybeClient of
    Nothing -> return clients
    Just clientTuple -> importNewClients controlChannel (clientTuple:clients)

readFromClient handle channel = do
  hPutStr handle "Prompt> "
  line <- hGetLine handle
  if line == header
    then do
      message <- readMultipleLines handle
      putStrLn $ "[client channel] " ++ message
      let format = checkFormat message
      case format of
        False -> hPutStrLn handle "Error! Bad message format."
        True  -> atomically $ writeTChan channel message
      readFromClient handle channel
    else do
      hPutStrLn handle "Error! Bad message format.2"
      readFromClient handle channel

readMultipleLines handle = do
  line1 <- hGetLine handle
  line2 <- hGetLine handle
  line3 <- hGetLine handle
  return $ unlines [header, line1, line2, line3]

writeToClient handle channel = do
  message <- atomically $ readTChan channel
  hPutStrLn handle message
  writeToClient handle channel
