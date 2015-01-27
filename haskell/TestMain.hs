import qualified Args
import qualified Chapter
import qualified Split
import qualified Time
import qualified XML
import UnitTest
import qualified ArgsTest
import qualified ChapterTest
import qualified SplitTest
import qualified TimeTest
import qualified XMLTest
import Control.Monad (when)
import System.Environment
import System.Process

testResultLists :: [[UnitTest.TestResult]]
testResultLists = [XMLTest.unitTestResults, SplitTest.unitTestResults, TimeTest.unitTestResults, ChapterTest.unitTestResults, ArgsTest.unitTestResults]

{-
 - Output unit test results to the console.
 - First parameter indicates whether or not to run in verbose mode.
 - Second parameter is the list of unit test results.
 -}
outputTestResults :: Args.Args -> IO ()
outputTestResults args = do  
  let rs = (concat testResultLists)
  let succeeded = allSuccesses rs
  let failed = allFailures rs
  putStrLn ("Successful unit tests: " ++ (show $ length succeeded))
  putStrLn ("Failed unit tests: " ++ (show $ length failed))
  -- If verbose, print the failed unit tests. 
  let verbose = Args.verbose args
  when verbose $ do
    mapM_ print failed

{-
 - Main function.
 -}
main = do
  -- Get command line arguments and parse them.
  args <- fmap Args.getArgCollection getArgs
  putStrLn ("Arguments: "++ show args ++ "\n")
  -- Run unit tests and output the results to the console.
  outputTestResults args 
