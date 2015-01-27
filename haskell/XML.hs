{- 
 - A simple XML parser. 
 -}
module XML
(
  Lexeme(..)
 ,Section(..)
 ,Chapter(..)
 ,makeLexeme
 ,getLexemes
 ,getSections
) where

import Data.Char (isSpace)
import qualified Time
import Control.Applicative
import Control.Monad.State

-- Represents an XML tag: <foo> or </foo>
data Lexeme = BeginTag String 
         | EndTag String
         | Content String 
  deriving(Show,Eq)

data Section = Sec String [Section] 
             | El String deriving(Show,Eq)

data Chapter = Chapter 
  {
    uid   :: Maybe String
   ,start :: Maybe Time.Time
   ,end   :: Maybe Time.Time
   ,name  :: Maybe String
  } deriving(Show,Eq)

{-
 - Group a list of lexemes into a list of top-level XML sections.
 - Each section may contain a tree of subsections.
 -}
getSections :: [Lexeme] -> [Section]
getSections = evalState getSectionsM
  
{-
 - Parse a string into a list of XML lexemes.
 - Use the State monad to iterate through the string.
 -}
getLexemes :: String -> [Lexeme]
getLexemes = map makeLexeme . evalState getParts
  
-- Turn a string into an XML lexeme.
makeLexeme :: String -> Lexeme
makeLexeme ('<':'/':cs) = EndTag cs
makeLexeme ('<':cs)     = BeginTag cs
makeLexeme cs           = Content cs
  
{-
 - Parse a beginning state into a list of things. 
 - Parameter 'getFirst' is a function (State monad) that extracts the first thing from a non-empty state. 
 - Parameter 'isEmpty' is a function used to determine whether the state is effectively empty.
 - Use State monad to read a string as an input stream.
 - The 'state' is the remainder of the string.
 -}
parse :: MonadState s m => (s -> Bool) -> m a -> m [a]
parse isEmpty getFirst = do 
  -- Check if the state is empty.
  xs <- get
  if (isEmpty xs)
     then return [] -- State is effectively empty. No more things.
     else do 
       -- Get the first thing, call recursively to get the rest of the things, then return the combination. 
       t  <- getFirst
       ts <- parse isEmpty getFirst
       return (t:ts)

{-
 - Group a list of lexemes into a list of top-level XML sections.
 - Each section may contain a tree of subsections.
 -}
getSectionsM :: State [Lexeme] [Section]
getSectionsM = parse (==[]) firstSectionM
        
{-
 - Parse a string into a list of XML lexemes (represented as strings)
 -}
getParts :: State String [String]
getParts = parse (all isSpace) firstPart
       
{-
 - Extract the first coherent section from a list of lexemes. 
 - Pre-requisite: The list of lexemes must be non-empty.
 -}
firstSectionM :: State [Lexeme] Section
firstSectionM = do
  -- Separate the head from the rest of the list.
  first <- stateHead
  case first of 
    (Content text)   -> returnWithContext (El text) -- Found content; return element section.
    (BeginTag label) -> do                          -- Found the beginning of a section; extract it.
      subSections <- getSubsections
      returnWithContext (Sec label subSections)

{-
 - Extract the contents (subsections) of the first coherent section from a list of lexemes. 
 -}
getSubsections :: State [Lexeme] [Section]
getSubsections = do
  xs <- get
  if xs == [] 
    then return [] -- Reached the end of the XML
    else do 
      -- Separate the head from the rest of the list.
      t <- stateHead
      case t of 
        -- Reached the end of the section
        (EndTag _)    -> returnWithContext [] 
        -- Found content; add it and continue recursively.
        (Content tx)  -> do                   
          ss <- getSubsections 
          returnWithContext (El tx:ss) 
        -- Found a subsection. 
        (BeginTag label) -> do         
          subSections  <- getSubsections -- Extract contents of this subsection
          moreSections <- getSubsections -- Continue to find more sections.
          returnWithContext (Sec label subSections:moreSections)
  
{-
 - Extract the first XML lexeme of a string.
 -}
firstPart :: State String String
firstPart = do
  -- Trim whitespace from the stored string.
  modify $ dropWhile isSpace
  ys <- get
  case ys of 
    -- If we meet an opening brace, then extract everything up until the first closing brace. 
    ('<':_) -> state $ breakExclusive (=='>')
    -- Else extract everything up until the first opening brace. 
    _       -> state $ break (=='<')

{-
 - Works like the standard library function 'break', 
 - except it drops the first element from the second half of the list 
 - (i.e., the first element satisfying the prerequisite). 
 -}
breakExclusive :: (a -> Bool) -> [a] -> ([a], [a])
breakExclusive p xs = (y, drop 1 ys)
  where (y,ys) = break p xs

{-
 - A stateful computation (using State monad) that extracts the head of the state. 
 - The new state will be the tail of the old state.
 - Pre-requisite: The state must be a non-empty list.
 -}
stateHead = state $ \s -> (head s, tail s)
  
{-
 - A stateful computation (using State monad) that returns its argument along with the current state.
 -}
returnWithContext :: MonadState s m => a -> m a
returnWithContext e = state $ \s -> (e,s)
