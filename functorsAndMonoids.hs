import Data.Char  
import Data.List
import Control.Applicative 
import Data.Monoid
import qualified BinaryTree as B
import qualified Data.Foldable as F  

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

          putStr "Enter two more lines: \n"
          a <- fmap (map toUpper) ((\a b -> a ++ " " ++ b ++ "?!") <$> getLine <*> getLine)
          putStrLn $ "The two lines concatenated turn out to be: " ++ a 

myFunc = (map (*10))

weirdShit = (zipWith (+)) <$> (myFunc) <*> (myFunc) 
-- weirdShit [1..5]
-- [20,40,60,80,100]

wtf = (myFunc) <$> (weirdShit)
-- wtf [1..5]
-- [200,400,600,800,1000]

moreWtf = wtf <$> (myFunc)
-- moreWtf [1..5]
-- [2000,4000,6000,8000,10000]

evenMoreWtf one two = (+) <$> one <*> two
-- evenMoreWtf [1..5] [1..3]
-- [2,3,4,3,4,5,4,5,6,5,6,7,6,7,8]

evenMoreMoreWtf a b c = a <$> b <*> c
-- evenMoreMoreWtf (^) [1..2] [1..2]
-- [1, 1, 2, 4] -> [1^1, 1^2, 2^1, 2^2]
-- evenMoreMoreWtf (^) [1..5] [2, 10]
-- [1^2, 1^10, 2^2, 2^10 ... 5^2, 5^10] -> [1,1,4,1024,9,59049,16,1048576,25,9765625]

zippingLists a b c = getZipList $ (\a b c -> a + b + c) <$> ZipList a <*> ZipList b <*> ZipList c 

divisibleBy :: Int -> Int -> Bool
divisibleBy b a = a `mod` b == 0

cool = sequenceA [(>4),(<10),odd] 7

getNumbers = filter (and . (sequenceA [(divisibleBy 13),(divisibleBy 7), (divisibleBy 3), (divisibleBy 5)])) [1..10000]  
-- [1365,2730,4095,5460,6825,8190,9555] 
-- those are the numbers less than 10000 which are divisible by 13, 7, 3 and 5

newtype Pair b a = Pair { getPair :: (a,b) } deriving (Show)
instance Functor (Pair c) where  
    fmap f (Pair (x,y)) = Pair (f x, y)  

mappingOverPairs = getPair $ fmap (*100) (Pair (100, "Hello"))

newtype Person name age = Person { getPerson :: (name, age)} deriving (Show) 
instance Functor (Person b) where 
    fmap f (Person (name, age)) = Person (name, f age)

birthday person = fmap (+1) person
rico = Person ("Rico", 21)
-- getPerson $ birthday rico 
-- ("Rico", 22)

type StringSorter = String -> String -> Ordering 
lengthCompare :: StringSorter 
lengthCompare x y = (length x `compare` length y) `mappend`  
                    (vowels x `compare` vowels y) `mappend`
                    (x `compare` y)  
        where vowels = length . filter (`elem` "aeiou")

sortStrings :: StringSorter -> [String] -> [String] 
sortStrings func strings = sortBy (func) strings 

-- sortStrings (lengthCompare) ["hello", "my", "friend", "a", "b", "aaaaa"]
-- ["b","a","my","hello","aaaaa","friend"]
-- notice that a comes after b - because both have the same length but then "a" has more vowels than b so
-- it is greater and get's pushed to the right. 

instance F.Foldable B.Tree where  
    foldMap f B.EmptyTree = mempty  
    foldMap f (B.Node x l r) = F.foldMap f l `mappend`  
                             f x           `mappend`  
                             F.foldMap f r  

testTree = B.Node 5  
            (B.Node 3  
                (B.Node 1 B.EmptyTree B.EmptyTree)  
                (B.Node 6 B.EmptyTree B.EmptyTree)  
            )  
            (B.Node 9  
                (B.Node 8 B.EmptyTree B.EmptyTree)  
                (B.Node 10 B.EmptyTree B.EmptyTree)  
            )  

condense tree = F.foldMap (\x -> [x]) tree 
hasValueOver tree val = getAny $ F.foldMap (\x -> Any $ x > val) tree 
filterTree func = filter (func) . condense 