module Switch.Message
( checkFormat
, readDestination
, createMessage
, header
) where

import Switch.Types

import Text.Regex.Posix
import Text.Read (readMaybe)

-- message format
zerothLine = "SWITCH MESSAGE"
firstLine = "Source: "
secondLine = "Destination: "

header = zerothLine

zerothLineRE   = zerothLine
firstLineRE    = "[sS]ource:"
secondLineRE   = "[dD]estination:"
thirdLineRE    = ""                  -- no one cares about third line

--TODO: add source address check
-- T stands for Template
checkFormat :: Message -> Bool
checkFormat message =
  let messageLines = lines message
  in  if  length messageLines < 4
        then False
        else let  (_,_,sourceAddrStr) = messageLines !! 1 =~ firstLineRE :: (String, String, String)
                  (_,_,destAddrStr)   = messageLines !! 2 =~ secondLineRE :: (String, String, String)
                  sourceAddr = readMaybe sourceAddrStr :: Maybe Address
                  destAddr   = readMaybe destAddrStr :: Maybe Address
             in  if sourceAddr /= Nothing && destAddr /= Nothing
                    then True
                    else False


readDestination :: Message -> Address
readDestination message =
  let messageLines = lines message
      (_,_,destAddrStr)   = messageLines !! 2 =~ secondLineRE :: (String, String, String)
  in read destAddrStr :: Address


-- Source -> Destination -> Message Text -> Whole message
createMessage :: Address -> Address -> Maybe String -> Message
createMessage src dest (Just text) =
  let srcLine = firstLine ++ show src
      destLine = secondLine ++ show dest
  in unlines [zerothLine, srcLine, destLine, text]

createMessage src dest Nothing = createMessage src dest (Just "text")
