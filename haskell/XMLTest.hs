module XMLTest
(
  unitTestResults
) where

import UnitTest
import XML

makeLexemeTest :: TestCollection Lexeme
makeLexemeTest = 
  [
    ("makeLexeme 1", Test (makeLexeme "<foo") (BeginTag "foo"))
   ,("makeLexeme 2", Test (makeLexeme "</foo") (EndTag "foo"))
   ,("makeLexeme 3", Test (makeLexeme "<foo") (BeginTag "foo"))
   ,("makeLexeme 4", Test (makeLexeme "<foo") (BeginTag "foo"))
  ]

getLexemesTest :: TestCollection [Lexeme]
getLexemesTest = 
  [
    ("getLexemes 00", Test (getLexemes "") [])
   ,("getLexemes 01", Test (getLexemes "<foo>") [BeginTag "foo"])
   ,("getLexemes 02", Test (getLexemes "<foo><bar>") [BeginTag "foo", BeginTag "bar"])
   ,("getLexemes 03", Test (getLexemes "<foo>  <bar>") [BeginTag "foo", BeginTag "bar"])
   ,("getLexemes 04", Test (getLexemes "<foo><bar></foo>") [makeLexeme "<foo", makeLexeme "<bar", makeLexeme "</foo"])
   ,("getLexemes 05", Test (getLexemes "<foo> \n\n </foo>") [BeginTag "foo", EndTag "foo"])
   ,("getLexemes 06", Test (getLexemes "<foo>baz<bar>") [BeginTag "foo", Content "baz", BeginTag "bar"])
   ,("getLexemes 07", Test (getLexemes "<foo>  baz<bar>") [BeginTag "foo", Content "baz", BeginTag "bar"])
   ,("getLexemes 08", Test (getLexemes "foobar") [Content "foobar"])
   ,("getLexemes 09", Test (getLexemes "foo<bar>") [Content "foo", BeginTag "bar"])
   ,("getLexemes 10", Test (getLexemes "<ChapterTimeEnd>00:02:11.331200000</ChapterTimeEnd>") 
     [BeginTag "ChapterTimeEnd", Content "00:02:11.331200000", EndTag "ChapterTimeEnd"])
  ]

getSectionsTest :: TestCollection [Section] 
getSectionsTest = 
  [
    ("getSections 0", Test (getSections []) [])
   ,("getSections 1a", Test (getSections [Content "foo"]) [El "foo"])
   ,("getSections 1b", Test (getSections [BeginTag "foo", EndTag "foo"]) [Sec "foo" []])
   ,("getSections 2", Test (getSections [BeginTag "foo", BeginTag "bar", EndTag "foo"]) [Sec "foo" [Sec "bar" []]])
   ,("getSections 3", Test (getSections [BeginTag "foo", Content "bar", EndTag "foo"]) [Sec "foo" [El "bar"]])
   ,("getSections 4", Test (getSections [BeginTag "foo", BeginTag "bar", Content "baz", EndTag "bar", EndTag "foo"]) 
                                        [Sec "foo" [Sec "bar" [El "baz"]]])
   ,("getSections 5", Test (getSections [BeginTag "foo", EndTag "foo", BeginTag "bar", EndTag "bar"]) 
                                        [Sec "foo" [], Sec "bar" []])
   ,("getSections 6", Test (getSections (getLexemes 
     "<ChapterDisplay>\n<ChapterString>Chapter 01</ChapterString>\n<ChapterLanguage>eng</ChapterLanguage>\n</ChapterDisplay>")) 
     [Sec "ChapterDisplay" [Sec "ChapterString" [El "Chapter 01"], Sec "ChapterLanguage" [El "eng"]]])
  ]
  
unitTestResults :: [TestResult]
unitTestResults = 
  [
    results makeLexemeTest
   ,results getLexemesTest
   ,results getSectionsTest
  ]
