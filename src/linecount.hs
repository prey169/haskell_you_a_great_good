import System.Directory
import System.Environment
import System.IO

main = do (fileName:_) <- getArgs
          fileExists <- doesFileExist fileName
          if fileExists
          then do contents <- readFile fileName
                  putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
          else do putStrLn "The file doesn't exist!"
