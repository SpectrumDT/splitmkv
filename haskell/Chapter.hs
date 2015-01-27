module Chapter
(
  getChaptersFromXml
 ,getNamedSectionText
 ,getChapters
) where

import XML 
import qualified Time

{-
 - Search for a section with the given label.
 - Extract its subsections. 
 - If not found, return Nothing.
 -}
getNamedSection :: String -> [Section] -> Maybe [Section]
getNamedSection _ []        = Nothing                 -- Not found, default to Nothing
getNamedSection name ((Sec label subs):ss) 
  | label == name           = Just subs               -- Found the label we were looking for
  | otherwise               =                         -- No match; search subtree
      case getNamedSection name subs of 
           s@(Just _) ->      s                       -- Found match in subtree
           _          ->      getNamedSection name ss -- No match in subtree; search remaining sections
getNamedSection name (_:ss) = getNamedSection name ss -- No label; search remaining sections
   
{-
 - Search for a section with the given label.
 - If it contains a single element, then return the text of this element.
 - Else return Nothing.
 -}
getNamedSectionText :: String -> [Section] -> Maybe String
getNamedSectionText name ss = do
  [El text] <- getNamedSection name ss
  return text
       
{-
 - Take the contents of a section that ought to contain a chapter.
 - Extract chapter data and return a Chapter datastructure.
 -}
getChapter :: [Section] -> Chapter
getChapter ss = Chapter { 
  uid   = (getNamedSectionText "ChapterUID"       ss) 
 ,name  = (getNamedSectionText "ChapterString"    ss) 
 ,start = (getNamedSectionText "ChapterTimeStart" ss) >>= Time.parseTime
 ,end   = (getNamedSectionText "ChapterTimeEnd"   ss) >>= Time.parseTime
}

{-
 -Extract chapters from a section (and its subtree). 
 -}
getChapters' :: Section -> [Chapter]
getChapters' (El _)                 = []              -- Reached the root and found nothing. 
getChapters' (Sec "ChapterAtom" ss) = [getChapter ss] -- Chapter found. Extract it.
getChapters' (Sec _ ss)             = getChapters ss  -- Non-chapter section found. Search for chapters inside.

{- 
 - Extract chapters from a bunch of sections. 
 -}
getChapters :: [Section] -> [Chapter]
-- Use list monad to get chapters from all sections and concatenate them into one big list.
getChapters ss = ss >>= getChapters'

{-
 - Extract chapters from an XML string. 
 -}
getChaptersFromXml :: String -> [Chapter]
getChaptersFromXml = getChapters . getSections . getLexemes
