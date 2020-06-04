isYournameJohn :: [Char] -> Bool
isYournameJohn name = if (name == "John") then True else False 

removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase str = [ a | a <- str, a `elem` ['A'..'Z']]

firstLetter :: String -> Char
firstLetter str = str !! 0

factorial :: Integer -> Integer
factorial n = product [1..n]

factorial' :: Int -> Int
factorial' n = product [1..n]

circumference :: Float -> Float  
circumference r = 2 * pi * r  

circumference' :: Double -> Double  
circumference' r = 2 * pi * r 

toString number = show number

parseInt :: [Char] -> Int
parseInt string = read string

parseFloat :: [Char] -> Float
parseFloat string = read string

explicitType string = read string :: [Int]
thisIsTrue = explicitType "[1,2,3]" == [1,2,3]

getSuccessor thing = succ thing
getPredecessor thing = pred thing 

isYournameJohn' :: String -> String  
isYournameJohn' "John" = "Yes, you are John!"  
isYournameJohn' x = "Sorry, but you are not John." 

isThisEven :: (Integral a) => a -> String
isThisEven x 
    | x `mod` 2 == 0 = "Yeah my man, this is even!"
    | otherwise = "Sry bro, but this is not even."

fibonacci :: (Integral a) => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)