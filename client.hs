import Network (connectTo, PortID(PortNumber))
import System.IO (getLine, putStrLn, hGetLine, hPutStrLn)
import System.Environment (getArgs)
import Control.Concurrent (forkIO)

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
  hPutStrLn socket line
  loop socket

readLoop socket = do
  line <- hGetLine socket
  putStrLn line
  readLoop socket
