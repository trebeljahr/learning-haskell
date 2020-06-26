import System.IO  
import System.Directory 
import System.Environment   
import Data.List  
import Data.Maybe 

actions :: [(String, [String] -> IO ())]  
actions = [
    ("add", addTodo), 
    ("append", addTodo),
    ("remove", removeTodo), 
    ("delete", removeTodo),
    ("view", viewTodo),
    ("show", viewTodo)
    ]

main = do 
    (command:filePath:args) <- getArgs 
    fileExists <- doesFileExist filePath  
    case lookup command actions of
        Nothing -> putStr $ "Command " ++ command ++ " does not exist. Try add, remove or view!"
        Just action ->  if not fileExists then putStr $ "Todo File: " ++ filePath ++ " does not exist!"
                        else action $ [filePath] ++ args 


viewTodo (filePath:_) = do 
    contents <- readFile filePath
    let todoItems = addLineNumbers (lines contents)
    putStr todoItems 

addLineNumbers contents = unlines $ zipWith (\n line -> show n ++ ". " ++ line) [1..] contents

addTodo (filePath:item:_) = do 
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

removeTodo (filePath:number:_) = do 
    handle <- openFile filePath ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let todoTasks = lines contents 
    case reads number :: [(Int,String)] of 
        [(index,"")] -> if  ((index > length todoTasks) || (index < 0)) 
                        then putStr $ "You can only delete todos that you have. Specify the one you want to delete with a number between 1 and " 
                        ++ (show $ (length todoTasks)) ++ "\n" 
                        else do 
                            let newTodoItems = delete (todoTasks !! (index - 1)) todoTasks 
                            putStr $ addLineNumbers newTodoItems    
                            hPutStr tempHandle $ unlines newTodoItems  
                            hClose handle  
                            hClose tempHandle  
                            removeFile filePath  
                            renameFile tempName filePath
        _ -> putStr "Please enter a valid number for a todo you want to delete."


                            
   

