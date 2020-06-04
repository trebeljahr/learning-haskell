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

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2) 

addStringTuples :: (String, String) -> (String, String) -> (String, String)  
addStringTuples (x1, y1) (x2, y2) = (x1 ++ " " ++ y1, x2 ++ " " ++ y2) 

tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  

length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs  

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs 

product' :: (Num a) => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs 

capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

max' :: (Ord a) => a -> a -> a  
max' a b   
    | a > b     = a  
    | otherwise = b  

min' :: (Ord a) => a -> a -> a  
min' a b   
    | a < b     = a  
    | otherwise = b  

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2 
          (skinny, normal, fatty) = (18.5, 25.0, 30.0  )

a = [let square x = x * x in (square 5, square 3, square 2)] 
b = (let (a,b,c) = (1,2,3) in a+b+c) * 100

head' :: [a] -> a  
head' [] = error "No head for empty lists!"  
head' (x:_) = x  

head' :: [a] -> a  
head' xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x  

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list." 

describeList' :: [a] -> String  
describeList' xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list." 