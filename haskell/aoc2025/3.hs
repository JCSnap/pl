main :: IO ()
main = interact solve

solve :: String -> String
solve input = show $ foldl (\acc x -> acc + getLineMax x) 0 $ lines input
  where
    getLineMax line
      | length arrAfterMax > 0 = read (max : [maximum arrAfterMax]) :: Int
      | otherwise              = read (maximum arrBeforeMax : [max]) :: Int
      where
        max = maximum line
        arrAfterMax = tail $ dropWhile (/= max) line
        arrBeforeMax = takeWhile (/= max) line

