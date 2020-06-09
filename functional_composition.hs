quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let small = quicksort [a | a <- xs, a <= x]  
        big = quicksort [a | a <- xs, a > x]  
    in  small ++ [x] ++ big 

add :: (Num a) => a -> a -> a
add x y = x + y
-- add 10 10 == 10

add20to = add 20
--add20to 10 == 30
add40to = add 40
-- add40to 10 == 50
-- add20to (add40to (add20to 0)) == 80

divideBy5 = (/5) 
-- divideBy5 10 == 2.0
divideBy10 = (/10)
-- divideBy10 30 == 3.0 
-- divideBy5 (divideBy10 100) == 2.0

apply2Times :: (a -> a) -> a -> a
apply2Times f x = f (f x)
-- apply2Times divideBy10 1000 == 10.0

applyNTimes :: (Num b, Eq b, Ord b) => (a -> a) -> a -> b -> a
applyNTimes f res b
    | b <= 0 = res
    | b == 1 = f res
    | otherwise = applyNTimes (f) (f res) (b - 1)

-- applyNTimes (divideBy10) 100000 3 == 100.0
-- applyNTimes (drop 2) [1..10] 3 == [7,8,9,10]
-- applyNTimes (take 3) [1..100] 3 == []
-- applyNTimes (+10) 10 10 == 110

add10NTimes = applyNTimes (+10) 
-- this could also be called multiply by ten haha ^^ 

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]  
-- == ["foo fighters","bar hoppers","baz aldrin"] 

myList = [1..10]
-- filter (<= 10) myList == [a | a <- myList, a <= 10 ]

quicksort' :: (Ord a) => [a] -> [a]    
quicksort' [] = []    
quicksort' (x:xs) =     
    let smallerSorted = quicksort' (filter (<=x) xs)  
        biggerSorted = quicksort' (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted  

largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [100000,99999..])  
    where p x = x `mod` 3829 == 0  

-- this (\a -> a*a + x) is a lambda function
squareListAndAddX :: (Num a) => [a] -> a -> [a]
squareListAndAddX list x = map (\a -> a * a + x) list

-- foldl and foldr take a function and apply it to each element in a list 
-- accumulating the value. The result of each function invocation becomes the 
-- new acc for the next call with the next value in the list.
-- A starting value for acc is provided - in the sumUp case it is 0 in the multUp it is 1.
-- foldl starts from the left, foldr from the right. 
sumUp :: (Num a)=>[a] -> a
sumUp list = foldl (\acc x -> acc + x) 0 list


-- that is the same as a factorial function
multUp :: (Num a)=>[a] -> a
multUp list = foldr (\x acc -> acc * x) 1 list

-- foldr takes the accumulator from the right, and foldl from the left! 
-- notice the difference: 
-- foldr (\x acc -> newAcc) initAcc list  vs. foldl (\acc x -> newAcc) initAcc list
stringConcat :: [String] -> String
stringConcat list = foldl (\acc x -> acc ++ " " ++ x) "This is the acc string:" list 
-- stringConcat ["Hello","world"] == "This is the acc string: Hello world"


wrongStringConcat :: [String] -> String
wrongStringConcat list = foldl (\x acc -> acc ++ " " ++ x) "This is the acc string:" list  
-- wrongStringConcat ["Hello", "world"] == "world Hello This is the acc string:"

reverseList :: [a] -> [a]
reverseList list = foldl (\acc x -> x : acc) [] list

reverseList' :: [a] -> [a]
reverseList' list = foldl (flip (:)) [] list

sum' :: (Num a) => [a] -> a
sum' = foldr1 (+)

mult' :: (Num a) => [a] -> a
mult' = foldr1 (*)

-- the $ operator is kinda crazy.
rootOfSum = sqrt $ 1 + 2 + 3 + 3
-- rootOfSum == 3.0 

-- it can spare lots of parentheses.
rootOfSum' = sqrt (1 + 2 + 3 + 3)

wow = map ($ 3) [(4+), (10*), (^2), sqrt]  
-- wow == [7.0,30.0,9.0,1.7320508075688772]  

-- composing functions... with dots. -> these => .
multBy5ThenSquareThenAdd5 = (+5) . (^2) . (*5)
-- multBy5ThenSquareThenAdd5 1 == 30 -> (1 * 5)² + 5

volumeSphere :: Float -> Float
volumeSphere r = 4 / 3 * pi * r ^ 3

-- first cube input, then mult with pi, then mult with 4/3
volumeSphere' :: Float -> Float
volumeSphere' = (* (4 / 3)) . (* pi) . (^ 3)

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

fancy = replicate 10 . product . map (*2) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]
