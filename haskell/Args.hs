module Args
(
  Args(..)
 ,InFileType(..)
 ,parseArg
 ,getArg
 ,getVerbose
 ,getArgCollection
 ,getInFileType
) where

import XML
import qualified Time
import qualified Data.Char
import qualified Data.List
import qualified Data.Maybe
import qualified Data.String.Utils

data Args = Args 
  {
    verbose     :: Bool         -- Verbose mode. 
   ,test        :: Bool         -- Test mode. If true, do not generate any files. 
   ,inFile      :: Maybe String -- Source file to be split.
   ,chapterFile :: Maybe String -- XML file containing chapter information.
   ,artist      :: Maybe String -- Artist tag to insert on extracted files.
   ,album       :: Maybe String -- Album tag to insert on extracted files.
   ,beginAt     :: Maybe Int    -- The first chapter to extract (1-indexed).
   ,endAt       :: Maybe Int    -- The last chapter to extract (1-indexed).
  } deriving(Show,Eq)
  
data InFileType = MKV | WAV

{-
 - Parse command line arguments and return a nice DTO. 
 -}
getArgCollection :: [String] -> Args
getArgCollection args = Args
  {
    verbose     = getVerbose args
   ,test        = getBoolArg "-t" True args
   ,inFile      = getArg "-in" args
   ,chapterFile = getArg "-ch" args
   ,artist      = getArg "-a"  args
   ,album       = getArg "-l"  args
   ,beginAt     = parseArg "-b" args
   ,endAt       = parseArg "-e" args
  }
  
{-
 - Search the arguments for an argument heading 's'. 
 - Use Maybe monad. 
 - If found, then return the element AFTER 's'.
 - For example, if we search for "-in" and the arguments contain "-in foo.mkv", then return "foo.mkv". 
 -}
getArg :: String -> [String] -> Maybe String
getArg s args = do
  (_:a:_) <- Just (dropWhile (\t -> t /= s) args) -- Find the first occurrence of 's'
  return a                                        -- Extract the element AFTER 's', if possible. 

{-
 - Parse a Bool from a String. 
 - Not case sensitive.
 -}
parseBool :: String -> Maybe Bool
parseBool t = case (map Data.Char.toLower t) of
                   "true"  -> Just True
                   "false" -> Just False
                   _       -> Nothing
  
{-
 - Get a boolean argument. 
 - If not supplied or not recognizable as a bool, use use the default option provided.
 -}
getBoolArg :: String -> Bool -> [String] -> Bool
getBoolArg s defaultValue args = 
  case getArg s args of
       Just t  -> case (parseBool t) of 
                       Just b -> b
                       Nothing -> defaultValue
       Nothing -> defaultValue

{-
 - Get an argument of type a. 
 - Use Maybe monad. 
 -}
parseArg :: (Read a) => String -> [String] -> Maybe a
parseArg s args = do
  t        <- getArg s args
  [(n,"")] <- Just (reads t)
  return n

{-
 - Get the "verbose" flag. 
 - If the arguments contain "-v true", then run verbose. 
 - By default, verbose is false.
 -}
getVerbose :: [String] -> Bool
getVerbose = getBoolArg "-v" False

{-
 - Get the type of the input file: MKV or WAV.
 -}
getInFileType :: Args -> Maybe InFileType
getInFileType args = do 
  fileName <- inFile args
  let lower = (map Data.Char.toLower fileName)
  if (Data.String.Utils.endswith ".mkv" lower)
     then return MKV
     else if (Data.String.Utils.endswith ".wav" lower)
       then return WAV
       else Nothing
