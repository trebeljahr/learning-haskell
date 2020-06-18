-- using import statements
import Data.List
import Data.Function (on)
-- import only specific functions
import Data.Bool (otherwise, not)
-- import everything except specific function
import Data.String hiding (words)
-- import with prefix use - to prevent name clashes
import qualified Data.String
import qualified Data.List.NonEmpty as DLN 

-- now functions from Data.list like nub are accessible. 
removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates = nub

-- use words function from string library. Notice the prefix.
countWords :: String -> Int
countWords = length . Data.String.words

-- the other functions of the string library were imported 
-- without prefix and can be used without it, like lines below.
countLines :: String -> Int 
countLines = length . lines 

-- using shortened qualified import syntax.
intersperse' = DLN.intersperse 

sortingListsByLength :: [[a]] -> [[a]]
sortingListsByLength xs = sortBy (compare `on` length) xs 

-- sortingListsByLength [[1,2,3],[1,2],[1]]
-- [[1],[1,2],[1,2,3]]

-- there are set theory operators. 
-- union 
takeUnion list1 list2 = list1 `union` list2 

-- difference 
takeDifference list1 list2 = list1 \\ list2 
-- difference takes away only one of each element. not every matching. 
-- [x | xs <- replicate 3 [1..5], x <- xs] \\ [1..5]
-- [1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5] \\ [1,2,3,4,5] == [1,2,3,4,5,1,2,3,4,5]

-- intersect 
takeIntersection list1 list2 = list1 `intersect` list2

findThe5 list = findIndex (==5) list
-- returns nothing if there is no 5 in the list.
findTheLetter :: Char -> String -> [Int]    
findTheLetter letter string = findIndices (==letter) string 
-- findTheLetter 'h' "hello world how nice it is to have you here"
-- [0,12,30,39]


