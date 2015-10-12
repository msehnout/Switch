import Network (connectTo, PortID(PortNumber))
import System.IO (getLine, putStrLn, hGetLine, hPutStr)
import System.Environment (getArgs)
import Control.Concurrent (forkIO)

import Switch.Message (createMessage)
import Switch.Address (createRequest)

main :: IO()
main =  do
  arguments <- getArgs
  if length arguments >= 3
    then do
      let [host, portStr, addr] = take 3 arguments
      putStrLn $ "Starting client\nHost: " ++ host ++ "\nPort: " ++ portStr
      let port = fromIntegral (read portStr :: Int)
      sock <- connectTo host $ PortNumber port
      hPutStr sock $ createRequest (read addr :: Int)
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
