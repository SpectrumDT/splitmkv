import System.Process

main = do
  putStrLn "Run!"
  ph <- runCommand "ls .."
  exitCode <- waitForProcess ph
  putStrLn (show exitCode)