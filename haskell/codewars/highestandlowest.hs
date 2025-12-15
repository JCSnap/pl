convertArr :: String -> [Int]
convertArr input = map (\x -> read x :: Int) $ words input

highAndLow :: String -> String
highAndLow input = show (maximum $ convertArr input) ++ " " ++ show (minimum $ convertArr input)
