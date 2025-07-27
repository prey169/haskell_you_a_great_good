import Control.Monad

main = do
  colors <- forM [1,2,3,4] (\a -> do
    putStrLn $ "What colors do you associate with the number " ++ show a ++ "?"
    getLine)
  putStrLn "The colors you associate with 1, 2, 3, and 4 are:"
  mapM putStrLn colors
