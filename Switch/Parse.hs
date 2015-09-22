module Switch.Parse
( checkFormat
, readDestination
) where

import Switch.Types

import Text.Regex.Posix
import Text.Read (readMaybe)

-- message format
zerothLineT   = "SWITCH MESSAGE"
firstLineT    = "[sS]ource:"
secondLineT   = "[dD]estination:"
thirdLineT    = ""                  -- noone cares about third line

--TODO: add source address check
-- T stands for Template
checkFormat :: Message -> Bool
checkFormat message =
  let messageLines = lines message
  in  if  length messageLines < 4
        then False
        else let  (_,_,sourceAddrStr) = messageLines !! 1 =~ firstLineT :: (String, String, String)
                  (_,_,destAddrStr)   = messageLines !! 2 =~ secondLineT :: (String, String, String)
                  sourceAddr = readMaybe sourceAddrStr :: Maybe Address
                  destAddr   = readMaybe destAddrStr :: Maybe Address
             in  if sourceAddr /= Nothing && destAddr /= Nothing
                    then True
                    else False


readDestination :: Message -> Address
readDestination message =
  let messageLines = lines message
      (_,_,destAddrStr)   = messageLines !! 2 =~ secondLineT :: (String, String, String)
  in read destAddrStr :: Address
