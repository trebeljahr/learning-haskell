import System.IO  
  
main = do  
    putStr "Please enter the location you want to open: "
    filePath <- getLine
    handle <- openFile filePath ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle  


