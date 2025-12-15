import Data.Char

isPalindrome :: String -> Bool
check :: String -> Int -> Bool
check xs i
	| i >= length xs `div` 2 = True
	| otherwise = toLower (xs !! i) == toLower (xs !! (length xs - 1 - i)) && check xs (i + 1)

isPalindrome xs = check (filter isAlphaNum xs) 0

main = print (isPalindrome "A man, a plan, a canal: Panama")
