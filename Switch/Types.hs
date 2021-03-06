module Switch.Types
( Address
, Message
, AddressRespond(..)
, ControlMsg(..)
)where

type Address = Int
type Message = String

data AddressRespond = Accepted | NotAccepted deriving (Eq, Show)

data ControlMsg = RequestAddr | DeleteAddr deriving (Eq, Show)
