main :: IO ()
main = interact solve

splitOnChar :: Char -> String -> [String]
splitOnChar _ [] = [""]
splitOnChar delim (c:cs)
  | c == delim = "" : splitOnChar delim cs
  | otherwise  =
      let (x:xs) = splitOnChar delim cs
      in (c:x) : xs

solve :: String -> String
solve input = show $ foldl (\acc x -> if isInvalid x then acc + x else acc) 0 $ concat $ map generateRange $ map (\x -> splitOnChar '-' x) $ splitOnChar ',' input
	where
		generateRange (x:xs) = [(read x :: Int)..(read $ head xs :: Int)]
		isInvalid num = even len && take (len `div` 2) numStr == drop (len `div` 2) numStr
			where
				numStr = show num
				len = length $ show num
