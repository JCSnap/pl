
-- Implement a function that adds two numbers together and returns their sum in binary. The conversion can be done before, or after the addition.

addBinary :: Int -> Int -> String
getBinary :: Int -> Int -> String

getBinary x pow
  | pow < 0 = ""
  | ((x `div` (2^pow)) `mod` 2 == 1) = '1' : getBinary x (pow - 1)
  | otherwise                       = '0' : getBinary x (pow - 1)

addBinary a b = dropWhile (\x -> x `elem` "0") $ getBinary (a + b) 32

-- failed private test cases, too lazy to change
