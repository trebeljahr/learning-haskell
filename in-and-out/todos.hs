import System.IO  
import System.Directory  
import Data.List  
import System.Environment   

main = do 
    args <- getArgs  
    if (args !! 0 == "add") then addTodo (args !! 1) (args !! 2) 
    else if (args !! 0 == "delete") then removeTodo (read $ args !! 1) (args !! 2) 
    else if (args !! 0 == "view") then viewTodo (args !! 1) else return()


viewTodo filePath = do 
    contents <- readFile filePath
    let todoItems = addLineNumbers (lines contents)
    putStr todoItems 

addLineNumbers contents = unlines $ zipWith (\n line -> show n ++ ". " ++ line) [1..] contents

addTodo item filePath = do 
    handle <- openFile filePath ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let todoTasks = lines contents     
        newTodoItems = todoTasks ++ [item]    
    putStr $ addLineNumbers newTodoItems
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle  
    hClose tempHandle  
    removeFile filePath  
    renameFile tempName filePath

removeTodo number filePath = do 
    handle <- openFile filePath ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let todoTasks = lines contents     
        newTodoItems = delete (todoTasks !! (number-1)) todoTasks  
    putStr $ addLineNumbers newTodoItems    
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile filePath  
    renameFile tempName filePath

