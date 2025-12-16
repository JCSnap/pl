{-
[]                                -->  "no one likes this"
["Peter"]                         -->  "Peter likes this"
["Jacob", "Alex"]                 -->  "Jacob and Alex like this"
["Max", "John", "Mark"]           -->  "Max, John and Mark like this"
["Alex", "Jacob", "Mark", "Max"]  -->  "Alex, Jacob and 2 others like this"
-}
import Data.List (isPrefixOf)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace old new xs
  | old `isPrefixOf` xs = new ++ replace old new (drop (length old) xs)
  | otherwise           = head xs : replace old new (tail xs)

likes :: [String] -> String
likes [] = "no one likes this"
likes xs
	| length xs == 1 = xs !! 0 ++ " likes this"
	| length xs == 2 = xs !! 0 ++ " and " ++ (replace "likes" "like" $ likes (drop 1 xs))
	| length xs == 3 = xs !! 0 ++ ", " ++ (replace "likes" "like" $ likes (drop 1 xs))
	| otherwise = xs !! 0 ++ ", " ++ xs !! 1 ++ " and " ++ (show $ length $ drop 2 xs) ++ " others like this"

main = print (likes ["Alex", "Jacob", "Mark", "Max"])
