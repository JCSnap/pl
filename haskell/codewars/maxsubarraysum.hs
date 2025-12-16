{-
Input: [-2, 1, -3, 4, -1, 2, 1, -5, 4]
Output: 6 (Sum of [4, -1, 2, 1])
-}

maxSequence :: [Int] -> Int
maxSequence [] = 0
maxSequence xs = maximum $ 0 : map sum [take len (drop i xs) | i <- [0..length xs - 1], len <- [1..length xs - i]]
