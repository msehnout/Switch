import Control.Concurrent (forkIO, threadDelay)
import Control.Exception.Base (try, catch, SomeException)
import Control.Monad (replicateM, replicateM_)
import Control.Monad.Loops (whileJust_)
import Network (connectTo, PortID(PortNumber))
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (getLine, putStrLn, hGetLine, hPutStr, hClose)
import System.Random (randomRIO)

import Switch.Address (createRequest, readRespond)
import Switch.Log
import Switch.Message (createMessage, readMessage, readSource, readDestination, readText)
import Switch.Types (AddressRespond(..))
import Switch.TimeStamp (stringStamp)
import qualified Switch.Tags as Tg

main :: IO()
main =  do
  --get arguments from command line
  arguments <- getArgs
  if length arguments >= 3
    then do
      --Connect to switch server using given informations
      let [host, portStr, addr] = take 3 arguments
      printLog Tg.control $ "Starting client\nHost: " ++ host ++ "\nPort: " ++ portStr
      let port = fromIntegral (read portStr :: Int)
      sock <- connectTo host $ PortNumber port
      hPutStr sock $ createRequest (read addr :: Int)
      respond <- replicateM 2 (hGetLine sock)
      if (readRespond (unlines respond)) == Accepted
        then do
          forkIO $ readLoop sock addr
          repeatTimes <- randomRIO (20, 30) :: IO Int
          replicateM_ repeatTimes $ loop sock (read addr)
          exitProcedure sock
        else do
          printLog Tg.err "Address not accepted."
          exitProcedure sock
    else putStrLn "Wrong number of arguments!"
  where exitProcedure sock = do
          --hClose sock
          putStrLn "Exiting..."
          exitSuccess

--Send random messages in loop
loop socket addr = do
  delay_us <- randomRIO (1000, 1500) :: IO Int
  let delay_ms = delay_us*10^3
  threadDelay delay_ms
  dest <- randomDest
  text <- randomText
  --try to send message
  catch (hPutStr socket $ createMessage addr dest (Just text)) handler
  {-if num > 0
    then loop socket addr (num-1)
    else do
        hClose handle
        putStrLn "Exiting..."-}
  where
    handler :: SomeException -> IO ()
    handler ex = do
      putStrLn $ "Caught exception; exiting... "
      exitSuccess

readLoop socket addr = whileJust_ (readMessage socket) $ printMessage addr

printMessage addr msg = do
  let src = show . readSource $ msg
      dest = show . readDestination $ msg
      txt = readText msg
      msgDump = src ++ "->" ++ dest ++ ": " ++ txt
  if addr == dest
    then printLog "For me    " msgDump
    else printLog "Broadcast " msgDump

randomDest = randomRIO (0, 10) :: IO Int

randomText = do
    len <- randomRIO (5,15) :: IO Int
    text <- replicateM len (randomRIO('a','z') :: IO Char)
    return text
