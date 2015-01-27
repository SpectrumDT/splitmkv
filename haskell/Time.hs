module Time
(
  Time(..)
 ,parseTime
) where

import qualified Data.List.Split
import qualified Text.Printf

data Time = Time 
  {
    h  :: Integer
   ,m  :: Integer
   ,s  :: Integer
   ,ms :: Integer
  } deriving(Eq)

-- Parse a timestamp of the form MM:SS:SS:MS into a struct
parseTime :: String -> Maybe Time
parseTime t = 
  case Data.List.Split.splitOn "." t of
    [tt,mss] -> (case Data.List.Split.splitOn ":" tt of 
                      [hh,mm,ss] -> Just (Time{h = read hh, m = read mm, s = read ss, ms = read (take 3 mss)})
                      _          -> Nothing)
    _        -> Nothing

milliseconds :: Time -> Integer
milliseconds (Time{h = hh, m = mm, s = ss, ms = mss}) = mss + 1000 * (ss + 60 * (mm + 60 * hh))
    
makeTime :: Integer -> Time
makeTime mss = Time{h = h, m = m, s = s, ms = ms}
  where ms     = mss `mod` 1000
        totalS = mss `div` 1000
        s      = totalS `mod` 60
        totalM = totalS `div` 60
        m      = totalM `mod` 60
        h      = totalM `div` 60
        
negateTime :: Time -> Time
negateTime t = makeTime (-(milliseconds t))

addTime :: Time -> Time -> Time
addTime t1 t2 = makeTime (milliseconds t1 + milliseconds t2) 

multiplyTime :: Time -> Time -> Time
multiplyTime t1 t2 = makeTime (milliseconds t1 * milliseconds t2) 

absTime :: Time -> Time
absTime = makeTime . abs . milliseconds

signumTime :: Time -> Time
signumTime = makeTime . signum . milliseconds
    
instance Num Time where
  (+)         = addTime
  (*)         = multiplyTime
  negate      = negateTime
  abs         = absTime
  signum      = signumTime
  fromInteger = makeTime

instance Show Time where
  show t = hh ++ ":" ++ mm ++ ":" ++ ss ++ "." ++ mss
    where hh  = Text.Printf.printf "%02d" (h t)
          mm  = Text.Printf.printf "%02d" (m t)
          ss  = Text.Printf.printf "%02d" (s t)
          mss = Text.Printf.printf "%03d" (ms t)