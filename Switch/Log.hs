module Switch.Log
( printLog
) where

import Switch.TimeStamp (stringStamp)

printLog tag text = do
  timeStamp <- stringStamp
  putStrLn $ "t: " ++ timeStamp ++ " | [" ++ tag ++ "] " ++ text
