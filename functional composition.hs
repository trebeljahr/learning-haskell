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