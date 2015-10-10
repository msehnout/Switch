module Switch.Types
( Address
, Message
, AddressRespond(..)
)where

type Address = Int
type Message = String

data AddressRespond = Accepted | NotAccepted deriving (Show)
