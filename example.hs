main :: IO ()
main = putStrLn "Hello, World"

doubleMe x = x + x
doubleUs x y = 2*x + 2*y
betterDoubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 then x else 2*x
doubleSmallNumber' x = (if x > 100 then x else 2*x) + 1
lostNumbers = [1, 2, 3, 4, 5]
lostNumbersAndLostNumbers = lostNumbers ++ lostNumbers

stringsAreLists = ["h","e"] ++ ["l","l","o"]
a = stringsAreLists ++ ["w", "o", "r", "l", "d"]
stringConcat = "hello" ++ " " ++ "world"
getFirstElement x = x !! 0
getFirstChar = getFirstElement
getFirstChar' x = head x
compareLists = [2, 3, 1] > [2, 2, 1]
compareLists' = [2, 3, 1] > [2, 3]
compareLists'' = [1, 2, 3] == [1, 2, 3]
getListWithoutHead x = tail x
getLastElement x = last x
getEverythingButTheLastElement x = init x
getLength x = length x
checkForEmptyList x = null x
reverseList x = reverse x
takeElementsFromList y x = take y x
dropElementsFromList y x = drop y x
equalToTrue = takeElementsFromList 2 [1, 2, 3] == [1, 2]
equalToTrue' = dropElementsFromList 2 [1, 2, 3] == [3]
returnsTheMinimumIn x = minimum x
returnsTheMaximumIn x = maximum x
returnsTheSumOfElementsInList x = sum x
returnsTheProductOfElementsInList x = product x
saysIfSomethingIsInAList list something = something `elem` list
thisIsARange = [20..100]
thisIsAnotherRange = [1..10]
thisIsRangeCountingDown = [30, 29..10]
rangeOfLetters = ['a'..'p']
rangeWithSteps = [2, 3..100]
multiplesOf13' = [13,26..24*13]
multiplesOf13 = take 24 [13,26..]
creatingInfiniteLists = take 10 (cycle [2,3,4])
repeatingLists = take 5 (repeat 10)
replicatingLists = replicate 5 10 == [10, 10, 10, 10, 10]
setComprehension = [x*2 | x <- [1..10]]
setComprehensionResolvesTo = [2,4,6,8,10,12,14,16,18,20]
setComprehension' step lower upper = [x*step | x <- [lower..upper]]
filterSetComprehension = [x*2 | x <- [1..10], x*2 >= 12]
yeah = filterSetComprehension == [12,14,16,18,20]
filterSetComprehension' = [x | x <- [50..100], x `mod` 7 == 3]
yeah' = filterSetComprehension' == [52,59,66,73,80,87,94]
fizzBuzz upper = [if x `mod` 15 == 0 then "Fizz Buzz"
else if x `mod` 3 == 0 then "Fizz"
else if x `mod` 5 == 0 then "Buzz"
else show x | x <- [1..upper]]
boomBang xs = [if x <  10 then "BOOM!" else "BANG!" | x <- xs, odd x]
boomBangEvals = boomBang [7..13] == ["BOOM!","BOOM!","BANG!","BANG!"]
permutator = [x++y | x <- ["a", "b", "c"], y <- ["d", "e", "f"]]
permuteArray a = [x++y | x <- a, y <- a]
length' a = sum [1 | _ <- a]
list = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
nestedListComprehensions = [[x | x <- intermediate, even x ] | intermediate <- list]
z = nestedListComprehensions == [[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]
tuples = [(1,2), (3,4),(4,5)]
first = fst (tuples !! 0)
second = snd (tuples !! 0)
zippingThings = zip [1, 2, 3, 4] ["one", "two", "three", "four"]
zippingInfiniteThings = zip [1..] ["house", "car", "tree", "dog"]
allTheTriangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
-- Needed Triangles are those which are right triangles with a perimeter of exactly 24 and
-- side lengths less than or equal to 10.
neededTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]
