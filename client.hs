import Network (connectTo, PortID(PortNumber))
import System.IO (getLine, putStrLn, hGetLine, hPutStr)
import System.Environment (getArgs)
import Control.Concurrent (forkIO)

import Switch.Message (createMessage)
import Switch.Address (createRequest)

import System.Random (randomRIO)
--import Control.Concurrent.Thread.Delay (delay)

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
      loop sock $ read addr
    else putStrLn "Wrong number of arguments!"

loop socket addr = do
  _ <- getLine
  dest <- randomDest
  text <- randomText
  hPutStr socket $ createMessage addr dest (Just text)
  loop socket addr 

readLoop socket = do
  line <- hGetLine socket
  putStrLn line
  readLoop socket

randomDest = randomRIO (1, 10) :: IO Int 

randomText = do
    len <- randomRIO (5,15) :: IO Int
    text <- generateText len ""
    return text

generateText 0 str = return str

generateText x str = do
    char <- randomRIO('a','z') :: IO Char
    generateText (x-1) (char:str)

