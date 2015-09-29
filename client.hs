import Network (connectTo, PortID(PortNumber))
import System.IO (getLine, putStrLn, hGetLine, hPutStr)
import System.Environment (getArgs)
import Control.Concurrent (forkIO)

import Switch.Message (createMessage)

main :: IO()
main =  do
  arguments <- getArgs
  if length arguments >= 2
    then do
      let [host, portStr] = take 2 arguments
      putStrLn $ "Starting client\nHost: " ++ host ++ "\nPort: " ++ portStr
      let port = fromIntegral (read portStr :: Int)
      sock <- connectTo host $ PortNumber port
      forkIO $ readLoop sock
      loop sock
    else putStrLn "Wrong number of arguments!"

loop socket = do
  line <- getLine
  hPutStr socket $ createMessage 0 1 (Just line)
  loop socket

readLoop socket = do
  line <- hGetLine socket
  putStrLn line
  readLoop socket
