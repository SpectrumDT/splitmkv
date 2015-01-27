module TimeTest
(
  unitTestResults
) where

import UnitTest
import Time
import Control.Applicative

parseTimeTest :: TestCollection (Maybe Time)
parseTimeTest = 
  [
    ("parseTime 0", Test (parseTime "") Nothing)
   ,("parseTime 1", Test (parseTime "00:02:11.331200000") (Just (Time{h = 0, m = 2, s = 11, ms = 331})))
  ]

makeTimeTest :: TestCollection (Maybe Time)
makeTimeTest = 
  [
    ("makeTimeTest 0", Test (Just (fromInteger (61100))) ((parseTime "00:01:01.100")))
  ]

absTimeTest :: TestCollection (Maybe Time)
absTimeTest = 
  [
    ("absTimeTest 0", Test (abs <$> (parseTime "00:01:01.100")) ((parseTime "00:01:01.100")))
  ]

negateTimeTest :: TestCollection (Maybe Time)
negateTimeTest = 
  [
    ("negateTimeTest 0", Test (negate <$> (parseTime "00:00:00.000")) ((parseTime "00:00:00.000")))
   ,("negateTimeTest 1", Test (negate <$> (parseTime "00:01:00.000")) ((parseTime "-1:59:00.000")))
   ,("negateTimeTest 2", Test (negate <$> (parseTime "01:00:00.000")) ((parseTime "-1:00:00.000")))
  ]

subtractTimeTest :: TestCollection (Maybe Time)
subtractTimeTest = 
  [
    ("subtractTimeTest 0", Test ((-) <$> (parseTime "00:01:00.000") <*> (parseTime "00:01:00.000")) ((parseTime "00:00:00.000")))
   ,("subtractTimeTest 1", Test ((-) <$> (parseTime "00:02:11.331") <*> (parseTime "00:01:05.001")) ((parseTime "00:01:06.330")))
  ]
  
unitTestResults :: [TestResult]
unitTestResults = 
  [
    results parseTimeTest
   ,results makeTimeTest
   ,results absTimeTest
   ,results negateTimeTest
   ,results subtractTimeTest
  ]
