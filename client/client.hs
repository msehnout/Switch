import Network (connectTo, PortID(PortNumber))
import System.IO (getLine, putStrLn, hGetLine, hPutStr)
import System.Environment (getArgs)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception.Base (try, catch, SomeException)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

import Switch.Message (createMessage, readMessage, readSource, readDestination, readText)
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
      forkIO $ readLoop sock addr
      repeatTimes <- randomRIO (20, 30) :: IO Int
      loop sock (read addr) (repeatTimes)
    else putStrLn "Wrong number of arguments!"

loop socket addr num = do
  delay_us <- randomRIO (1000, 1500) :: IO Int
  let delay_ms = delay_us*10^3
  threadDelay delay_ms
  dest <- randomDest
  text <- randomText
  catch (hPutStr socket $ createMessage addr dest (Just text)) handler
  if num > 0
    then loop socket addr (num-1)
    else putStrLn "Exiting..."
  where
    handler :: SomeException -> IO ()
    handler ex = do
      putStrLn $ "Caught exception; exiting... "
      exitSuccess

readLoop socket addr = do
  maybeMsg <- readMessage socket
  case maybeMsg of
    Just msg -> do
      let src = show . readSource $ msg
          dest = show . readDestination $ msg
          txt = readText msg
          printMsg typeOfMsg = putStrLn $ typeOfMsg ++ src ++ "->" ++ dest ++ ": " ++ txt
      if addr == dest
        then printMsg "(For me) "
        else printMsg "(Broadcast) "
      readLoop socket addr
    Nothing -> do
      putStrLn $ "Sth wierd came to my socket..."

randomDest = randomRIO (0, 10) :: IO Int

randomText = do
    len <- randomRIO (5,15) :: IO Int
    text <- generateText len ""
    return text
    where
      generateText 0 str = return str
      generateText x str = do
          char <- randomRIO('a','z') :: IO Char
          generateText (x-1) (char:str)
