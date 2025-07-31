import System.Environment
import Data.List

main = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The Arguments are:"
  mapM_ putStrLn args
  putStrLn "The program name is:"
  putStrLn progName
  
