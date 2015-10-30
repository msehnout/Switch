module Switch.Tags
( err
, control
, client
) where

makeItTag xs = "[" ++ xs ++ "] "
err = makeItTag "error"
control = makeItTag "control"
client = makeItTag "client"
