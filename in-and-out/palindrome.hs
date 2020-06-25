main = interact $ palindrome 

palindrome :: String -> String 
palindrome input = 
    let inputLines = lines input
        reverseLines = map formatPalindrome inputLines  
    in unlines reverseLines

formatPalindrome :: String -> String 
formatPalindrome line = 
    let reversed = reverse line 
        found = if reversed == line then "a" else "not a"
    in "This line: \n" ++ line ++ " \nis " ++ found ++ " palindrome\n"
