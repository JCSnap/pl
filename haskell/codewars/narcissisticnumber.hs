narcissistic :: (Integral n, Show n) => n -> Bool
narcissistic n = fromIntegral (sum $ map (\x -> (read [x] :: Int) ^ (length $ show n)) $ show n) == n


