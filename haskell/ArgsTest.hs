module ArgsTest
(
  unitTestResults
) where

import UnitTest
import Args

getArgTest :: TestCollection (Maybe String)
getArgTest = 
  [
    ("getArg0", Test (getArg "foo" []) Nothing)
   ,("getArg1", Test (getArg "foo" ["foo"]) Nothing)
   ,("getArg2", Test (getArg "foo" ["foo","bar"]) (Just "bar"))
   ,("getArg3", Test (getArg "-foo" ["-foo","bar"]) (Just "bar"))
   ,("getArg4", Test (getArg "-foo" ["-foz","boz","-foo","bar","-foo","baz"]) (Just "bar"))
   ,("getArg5", Test (getArg "-fzz" ["-foz","boz","-foo","bar","-foo","baz"]) Nothing)
   ,("getArg6", Test (getArg "v" ["v","True"]) (Just "True"))
  ]

parseArgBoolTest :: TestCollection (Maybe Bool)
parseArgBoolTest = 
  [
    ("parseArgBoolTest0", Test (parseArg "v" ["v", "True"]) (Just True))
   ,("parseArgBoolTest1", Test (parseArg "v" ["v", "False"]) (Just False))
  ]

parseArgIntTest :: TestCollection (Maybe Int)
parseArgIntTest = 
  [
    ("parseArgIntTest0", Test (parseArg "v" ["v", "5"]) (Just 5))
  ]

getVerboseTest :: TestCollection Bool
getVerboseTest =
  [
    ("getVerbose0", Test (getVerbose []) False)
   ,("getVerbose1", Test (getVerbose ["v","true"]) False)
   ,("getVerbose2", Test (getVerbose ["-v","false"]) False)
   ,("getVerbose3", Test (getVerbose ["-v","troo"]) False)
   ,("getVerbose4", Test (getVerbose ["-v","true"]) True)
   ,("getVerbose5", Test (getVerbose ["-v","True"]) True)
   ,("getVerbose6", Test (getVerbose ["-v","TRUE"]) True)
  ]

unitTestResults :: [TestResult]
unitTestResults = 
  [
    results parseArgBoolTest
   ,results parseArgIntTest
   ,results getArgTest
   ,results getVerboseTest
  ]
