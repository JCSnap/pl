
createPhoneNumber :: [Int] -> String
createPhoneNumber xs = "(" ++ firstHalf ++ ")" ++ " " ++ take 3 secondHalf ++ "-" ++ drop 3 secondHalf
	where numStr = concat $ map (\x -> show x) xs
	      firstHalf = take 3 numStr
	      secondHalf = drop 3 numStr

main = print $ createPhoneNumber [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
