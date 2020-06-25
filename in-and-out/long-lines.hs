main = do 
    contents <- getContents
    putStr $ longLines' contents 

longLines :: String -> String
longLines input = 
    let inputLines = lines input 
        longLines = filter (\line -> length line > 10) inputLines
    in unlines longLines 

longLines' = unlines . filter ((>10) . length) . lines