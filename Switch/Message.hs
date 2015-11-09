module Switch.Message
( checkFormat
, readDestination
, readSource
, readText
, createMessage
, header
, readMessage
) where

import Switch.Types

import Control.Applicative
import Control.Exception.Base (try)
import Text.Read (readMaybe)
import Text.Regex.Posix
import System.IO (Handle, hGetLine)
import System.IO.Error (isEOFError)

-- message format
zerothLine = "SWITCH MESSAGE"
firstLine = "Source: "
secondLine = "Destination: "

header = zerothLine

-- Regular expressions to check message format
zerothLineRE   = zerothLine
firstLineRE    = "[sS]ource:"
secondLineRE   = "[dD]estination:"
thirdLineRE    = ""                  -- no one cares about third line

-- T stands for Template
checkFormat :: Message -> Bool
checkFormat message =
  let messageLines = lines message
  --check message length
  in  if  length messageLines < 4
        then False
        --use regexps to check message format
        else let  (_,_,sourceAddrStr) = messageLines !! 1 =~ firstLineRE :: (String, String, String)
                  (_,_,destAddrStr)   = messageLines !! 2 =~ secondLineRE :: (String, String, String)
                  sourceAddr = readMaybe sourceAddrStr :: Maybe Address
                  destAddr   = readMaybe destAddrStr :: Maybe Address
             in  if sourceAddr /= Nothing && destAddr /= Nothing
                    then True
                    else False

-- Read only destination address from the message
readDestination :: Message -> Address
readDestination message =
  let messageLines = lines message
      (_,_,destAddrStr)   = messageLines !! 2 =~ secondLineRE :: (String, String, String)
  in read destAddrStr :: Address

-- Read source address from the message
readSource :: Message -> Address
readSource message =
  let messageLines = lines message
      (_,_,sourceAddrStr)   = messageLines !! 1 =~ firstLineRE :: (String, String, String)
  in read sourceAddrStr :: Address

-- Read text(payload) from the message
readText :: Message -> String
readText message =
    let messageLines = lines message
    in messageLines !! 3

-- Create new message in client
-- Source -> Destination -> Message Text -> Whole message
createMessage :: Address -> Address -> Maybe String -> Message
createMessage src dest (Just text) =
  let srcLine = firstLine ++ show src
      destLine = secondLine ++ show dest
  in unlines [zerothLine, srcLine, destLine, text]

createMessage src dest Nothing = createMessage src dest (Just "text")

--IO function (not pure) to read message from users socket
readMessage :: Handle -> IO(Maybe String)
readMessage handle = do
  msgheader <- maybeReadLine handle
  case msgheader of
    --check one line if it is message header
    Just line -> if line == header
                    then do
                      --Read 3 lines within maybe context and concatenate them
                      msglines <- sequence $ take 3 $ repeat $ maybeReadLine handle
                      let addNewLines = map (\x-> fmap (++ "\n") x)
                          maybeFold = foldl (\acc x -> (++) <$> acc <*> x) (Just [])
                          maybeUnlines = maybeFold . addNewLines
                      return $ maybeUnlines (Just header:msglines)
                    else readMessage handle
    Nothing -> return Nothing

  where
    --Reading from user wrapped into error handling
    maybeReadLine handle = do
      input <- try $ hGetLine handle
      case input of
        Left e -> do
          if isEOFError e
             then do
                 return Nothing
             else do--ioError e
                 return Nothing
        Right line -> return $ Just line
