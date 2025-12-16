main :: IO ()
main = interact solve

solve :: String -> String
solve input = show $ foldl getFuel 0 $ map (\x -> read x :: Int) $ lines input
  where getFuel acc x
          | x == 0 = acc
          | otherwise = let left = maximum [x `div` 3 - 2, 0] in getFuel (acc + left) left


