import Data.Char  
import Data.List

-- instance Functor IO where  
--     fmap f action = do  
--         result <- action  
--         return (f result) 

main = do putStr "Enter a line: "
          line <- fmap (unwords . map reverse . words) getLine  
          putStrLn $ "Here it is word by word backwards: " ++ line 
          
          putStr "Enter another line: "
          line <- fmap (intersperse ' ' . map toUpper . (++ "!")) getLine  
          putStrLn $ "Here it is INTENSIFIED: " ++ line  
