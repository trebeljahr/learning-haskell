import System.IO 

main = do 
    putStr "Please enter a todo item to add: "
    todoItem <- getLine 
    appendFile "todos.txt" (todoItem ++ "\n")