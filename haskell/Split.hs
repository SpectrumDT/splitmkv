module Split
(
  getSplitCommands
) where

import XML
import Args
import qualified Time
import qualified Data.Char
import qualified Data.List
import qualified Data.Maybe

{-
 - Add numbers/indexes to a list.
 - For example, [a,b,c] becomes [(1,a),(2,b),(3,c)].
 -}
number :: [a] -> [(Int,a)]
number xs = zip [1..(length xs)] xs

{-
 - Extract the relevant chapters, as specified in the arguments. 
 -}
relevantChapters :: Args -> [a] -> [a]
relevantChapters args xs = take toTake $ drop toDrop $ xs
  where toDrop    = case (beginAt args) of 
                         (Just n) -> n-1       -- Drop all items before number n.
                         _        -> 0         -- Drop nothing.
        remaining = length xs                  -- The number of elements left after dropping some leading ones.
        toTake    = case (endAt args) of    
                         (Just n) -> n-toDrop  -- Take everything from 'toDrop' up to number n.
                         _        -> remaining -- Take all remaining elements.
        
getSplitCommand' :: Time.Time -> Time.Time -> String -> String -> String -> String -> String -> String
getSplitCommand' startTime endTime inFileName outFileName artistFlag albumFlag numberFlag = 
  let duration = endTime - startTime
  in "avconv -y -i " ++ inFileName ++ " -ss " ++ (show startTime) ++ " -t " ++ (show duration) ++ " -f flac - "++
          "| oggenc - -q 5 -o \"" ++ (outFileName ++ ".ogg") ++ "\"" ++ artistFlag ++ albumFlag ++ numberFlag
  
getSplitCommand :: Args -> (Int,Chapter) -> Maybe String
getSplitCommand args (n,c) = do
  -- Extract 'Maybe' values monadically
  inFileName  <- inFile args
  startTime   <- start c
  endTime     <- end c
  outFileName <- name c
  -- The next line falls back to Nothing if one of the three 'Maybe' values is Nothing.
  return (getSplitCommand' startTime endTime inFileName outFileName artistFlag albumFlag numberFlag)
  where artistFlag  = case artist args of Just s  -> " -a \"" ++ s ++ "\""
                                          Nothing -> ""
        albumFlag   = case album args of  Just s  -> " -l \"" ++ s ++ "\""
                                          Nothing -> ""
        numberFlag  = " -N " ++ show n
          
getSplitCommands :: Args -> [Chapter] -> [String]
getSplitCommands args = Data.Maybe.catMaybes . map (getSplitCommand args) . (relevantChapters args) . number
