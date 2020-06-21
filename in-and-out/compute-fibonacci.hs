main = do  
    putStrLn "Which fibonacci number do you want?"  
    number <- getLine  
    out number

fibs = 0:1: zipWith (+) fibs (tail fibs)

out :: String -> IO ()
out number = 
    let fib = show $ fibs !! (read number)
    in putStrLn ("Here is the " ++ number ++ ". Fibonacci number: " ++ fib)