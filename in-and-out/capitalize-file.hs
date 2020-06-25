import System.IO     
import Data.Char 

main = do 
    putStr "Please enter the location you want to open: "
    filePath <- getLine
    contents <- readFile filePath 
    putStr "Please enter the location you want to save to: "
    savePath <- getLine
    writeFile savePath (map toUpper contents)
    putStr "Success!"