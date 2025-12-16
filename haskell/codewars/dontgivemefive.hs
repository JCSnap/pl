-- small numbers
dontGiveMeFive :: Int -> Int -> Int
dontGiveMeFive start end = sum $ map hasFive [start..end]
	where hasFive x
		| any (\x -> x `elem` "5") (show x) = 0
		| otherwise = 1

main = print (dontGiveMeFive 4 17)

-- lazy to do optimised approach which requires some math
