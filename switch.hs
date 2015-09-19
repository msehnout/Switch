import Text.Read (readMaybe)
import Network
import System.IO (hGetLine, hPutStrLn, hPutStr)
import Control.Concurrent (forkIO)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChan, dupTChan, readTChan, tryReadTChan, writeTChan)

main :: IO()
main = do
  controlChannel <- atomically newTChan
  readChannel <- atomically newTChan
  let channels = (controlChannel, readChannel)
  socket <- listenOn $ PortNumber 4242
  forkIO $ switch channels []
  acceptLoop socket channels 0

acceptLoop socket channels address = do
  (handle,_,_) <- accept socket
  let controlChannel = fst channels
      readChannel    = snd channels
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
  let maybeChannel = lookup (read . head . words $ message) newClients
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
  message <- hGetLine handle
  putStrLn $ "[client channel] " ++ message
  let addr = readMaybe . head . words $ message :: Maybe Int
  case addr of
    Nothing -> hPutStrLn handle "Error! Bad message format."
    Just _ -> atomically $ writeTChan channel message
  readFromClient handle channel

writeToClient handle channel = do
  message <- atomically $ readTChan channel
  hPutStrLn handle message
  writeToClient handle channel
