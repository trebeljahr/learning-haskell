data Conversion = Number | Operator deriving (Show)

polishNotation :: String -> Float
polishNotation string = 
    let parts = (words string) 
        results = foldl handleStack [] parts
    in results !! 0

handleStack :: [Float] -> String  -> [Float]
handleStack stack next 
    | next == "+" = newStack ++ [(beforeLast + last)] 
    | next == "-" = newStack ++ [(beforeLast - last)]  
    | next == "/" = newStack ++ [(beforeLast / last)] 
    | next == "*" = newStack ++ [(beforeLast * last)] 
    | otherwise = stack ++ [(read next :: Float)]  
    where len = length stack 
          newStack = getNewStack stack 
          last = if len >= 2 then stack !! (len - 1) else 1
          beforeLast = if len >= 2 then stack !! (len - 2) else 1

getNewStack :: [Float] -> [Float]          
getNewStack stack = take (length stack - 2) stack