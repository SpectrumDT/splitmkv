module ChapterTest
(
  unitTestResults
) where

import UnitTest
import XML
import Chapter

getNamedSectionTextTest :: TestCollection (Maybe String)
getNamedSectionTextTest = 
  [
    ("getNamedSectionText 0", Test (getNamedSectionText "" []) Nothing)
   ,("getNamedSectionText 1", Test (getNamedSectionText "foo" []) Nothing)
   ,("getNamedSectionText 2", Test (getNamedSectionText "foo" [El "foo"]) Nothing)
   ,("getNamedSectionText 3", Test (getNamedSectionText "foo" [Sec "foo" []]) Nothing)
   ,("getNamedSectionText 4", Test (getNamedSectionText "foo" [Sec "foo" [Sec "bar" []]]) Nothing)
   ,("getNamedSectionText 5", Test (getNamedSectionText "foo" [Sec "foo" [El "bar"]]) (Just "bar"))
   ,("getNamedSectionText 6", Test (getNamedSectionText "foo" [Sec "baz" [El "bar"]]) Nothing)
  ]

unitTestResults :: [TestResult]
unitTestResults = 
  [
    results getNamedSectionTextTest
  ]
