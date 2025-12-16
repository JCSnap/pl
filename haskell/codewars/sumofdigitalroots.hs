
digitalRoot :: Integral a => a -> a
digitalRoot num
	| num < 10 = num
	| otherwise = digitalRoot (sumDigits num)
	where
		sumDigits 0 = 0
		sumDigits x =
			let (q, r) = x `divMod` 10
			in r + sumDigits q

