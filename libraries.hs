-- using import statements
import Data.List
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
