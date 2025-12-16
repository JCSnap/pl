main :: IO ()
main = interact solve

solve :: String -> String
solve input = show $ foldl (\acc x -> acc + x `div` 3 - 2) 0 $ map (\x -> read x :: Int) $ lines input


