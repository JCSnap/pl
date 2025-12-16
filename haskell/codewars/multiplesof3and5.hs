-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

solution :: Integer -> Integer
solution num
	| num - 1 < 3 = 0
	| otherwise = cur + solution (num - 1)
	where cur = if (num - 1) `mod` 3 == 0 || (num - 1) `mod` 5 == 0 then (num - 1) else 0

main = print (solution 200)
