module Switch.TimeStamp
( stringStamp
) where

import Data.Time
import Text.Regex.Posix

stringStamp :: IO(String)
stringStamp = do
    time <- getZonedTime
    let (_,str,_) = show time =~ "[0-9]+:[0-9]+:[0-9]+" :: (String,String,String)
    return str
