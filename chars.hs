import Data.Char
import Data.Function (on)
import Data.List (groupBy)

checkForAlphaNums :: String -> Bool
checkForAlphaNums string = all isAlphaNum string

words' :: String -> [String]
words' = filter (not . any isSpace) . groupBy((==) `on` isSpace)
-- words' "hello world" == ["hello", "world"]

getCategory :: Char -> GeneralCategory
getCategory character  = generalCategory character

charToInt :: Char -> Int 
charToInt character = digitToInt character 

intToChar :: Int -> Char 
intToChar integer = intToDigit integer

-- intToChar 10 == 'a'
-- charToInt 'a' == 10
-- because - hexadecimals. 

-- that is neat. 
cesarChiffre :: String -> Int -> String 
cesarChiffre string shift = map (chr . (+ shift) . ord)  string 

decodeCesar :: String -> Int -> String 
decodeCesar string shift = cesarChiffre string (negate shift)