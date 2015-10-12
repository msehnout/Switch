module Switch.Address
( createRequest
, readRequest
, respondAccepted
, respondNotAccepted
, readRespond
) where

import Switch.Types
import Text.Read (readMaybe)
import Text.Regex.Posix

-- Address request:
requestHeader = "SWITCH REQUEST"
requestBody = "Address: "

-- Check request message regular expressions
requestBodyRE = "[aA]ddress:"

-- Create request message
createRequest :: Address -> Message
createRequest addr = unlines [requestHeader, requestBody ++ show addr]

-- Read the request on server side
readRequest :: Message -> Maybe Address
readRequest msg = 
  let [fstLine, sndLine] = take 2 $ lines msg
      -- fstLine = (lines msg) !! 0
      -- sndLine = (lines msg) !! 1
      fstMatch = fstLine == requestHeader
      (_,_,addrStr) = sndLine =~ requestBodyRE :: (String, String, String)
      requestedAddr = readMaybe addrStr :: Maybe Address
  in if fstMatch == False
        then Nothing
        else requestedAddr

-- Switch respond messages:
respondHeader = "SWITCH RESPOND"
respondBody = "Address "
respondAcc = "accepted"
respondNot = "not accepted"

-- Create accept message for switch server
respondAccepted :: Message
respondAccepted = unlines [respondHeader, respondBody ++ respondAcc]

-- Create not accepted message for switch server
respondNotAccepted :: Message
respondNotAccepted = unlines [respondHeader, respondBody ++ respondNot]

-- Read message on client side
readRespond :: Message -> AddressRespond
readRespond msg =
  let header = (lines msg) !! 0
      respond = (lines msg) !! 1
  in if header == respondHeader
        then let word1 = (words respond) !! 0
                 word2 = (words respond) !! 1
             in if word2 == respondAcc
                   then Accepted
                   else NotAccepted
        else NotAccepted
