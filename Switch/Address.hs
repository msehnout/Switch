module Switch.Address
( createRequest
, respondAccepted
, respondNotAccepted
) where

import Switch.Types

-- Address request:
requestHeader = "SWITCH REQUEST"
requestBody = "Address: "

-- Check request message regular expressions
requestHeaderRE = requestHeader
requestBodyRE = "[aA]ddress:"

-- Create request message
createRequest :: Address -> Message
createRequest addr = unlines [requestHeader, requestBody ++ show addr]

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
