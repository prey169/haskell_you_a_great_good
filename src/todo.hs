import System.IO

main = do
  putStrLn "Enter a TODO item:"
  todoItem <- getLine
  appendFile "todo.txt" (todoItem ++ "\n")
