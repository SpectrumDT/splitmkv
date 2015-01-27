import qualified Args
import qualified Chapter
import qualified Split
import qualified Time
import qualified XML
import Control.Monad (when)
import Control.Applicative ((<$>),(<*>),pure,Applicative)
import qualified GHC.IO.Handle
import qualified System.Environment
import qualified System.Process

{-
 - Output to the console the result of parsing the chapters file. 
 -}
outputParseResults :: Args.Args -> Maybe [XML.Chapter] -> Maybe [String] -> IO ()
outputParseResults args chapters splitCommands = do
  applyMaybe (\c -> putStrLn ("Chapters: "++ show (length c))) chapters
  -- If verbose, print the parsed chapters and split commands
  let verbose = Args.verbose args
  when verbose $ do
    putStrLn "\nChapters:"
    applyMaybe (mapM_ print) chapters
    putStrLn "\nSplit commands:"
    applyMaybe (mapM_ print) splitCommands
    return ()
  --applyMaybe (outputParseResults' args chapters) splitCommands
    
outputParseResults' :: Args.Args -> [XML.Chapter] -> [String] -> IO ()
outputParseResults' args chapters splitCommands = do
  putStrLn ("Chapters: "++ show (length chapters))
  -- If verbose, print the parsed chapters and split commands
  let verbose = Args.verbose args
  when verbose $ do
    putStrLn "\nChapters:"
    mapM_ print chapters
    putStrLn "\nSplit commands:"
    mapM_ print splitCommands
    
{-
 - Execute one split command.
 -}
performSplitCommand :: Args.Args -> String -> IO ()
performSplitCommand args command = do
  let verbose = Args.verbose args
  let testMode = Args.test args
  when verbose $ do
    putStrLn ("Executing command: "++ command)
  when (not testMode) $ do
    ph <- System.Process.runCommand command
    exitCode <- System.Process.waitForProcess ph
    putStrLn "Done!"

{-
 - Perform the actual file generation.
 - If the arguments tell us to, of course. 
 -}
performSplitCommands :: Args.Args -> [String] -> IO ()
performSplitCommands args splitCommands = do
  mapM_ (performSplitCommand args) splitCommands

{-
 - Extract chapter information from an MKV file.
 - Return the chapter XML as a string.
 -}
extractChapterXml :: String -> IO String
extractChapterXml inFileName = do
  let command = ("mkvextract chapters "++inFileName)
  -- Run mkvextract.
  (stdin,stdout,stderr,ph) <- System.Process.runInteractiveCommand command
  -- Wait for mkvextract to terminate.
  exitCode <- System.Process.waitForProcess ph
  -- Extract the contents of its stdout.
  GHC.IO.Handle.hGetContents stdout

{-
 - Take a Maybe value and a functor.
 - If the Maybe value is Just something, apply the functor to the the contents 
 - and wrap the result in Just - inside the functor. 
 - If the Maybe value is nothing, then return a Nothing wrapped inside the functor. 
 -}
applyMaybe :: Applicative m => (a -> m b) -> Maybe a -> m (Maybe b)
applyMaybe f = maybe (pure Nothing) (fmap Just . f)
      
{-
 - Get the XML describing the chapters of the MKV.
 - Extract this either from the MKV itself or from a stand-alone XML file,
 - depending on what the arguments specify. 
 -}
getChapterXml :: Args.Args -> IO (Maybe String)
getChapterXml args = do
  case (Args.getInFileType args) of 
    Just Args.WAV -> do
      applyMaybe readFile (Args.chapterFile args)
    Just Args.MKV -> do
      applyMaybe extractChapterXml (Args.inFile args)
    _ -> return Nothing

{-
 - Main function.
 -}
main = do
  -- Get command line arguments and parse them.
  args <- fmap Args.getArgCollection System.Environment.getArgs
  putStrLn ("Arguments: "++ show args ++ "\n")
  -- Read chapter file.
  chapterXml <- getChapterXml args
  -- Parse chapter file and generate split commands.
  let chapters = Chapter.getChaptersFromXml <$> chapterXml
  let splitCommands = (Split.getSplitCommands args) <$> chapters
  -- Output parsed result to console
  outputParseResults args chapters splitCommands 
  -- Perform file generation.
  applyMaybe (performSplitCommands args) splitCommands 
