-- For example, when an array is passed like [19, 5, 42, 2, 77], the output should be 7.
import Data.List

sumTwoSmallestNumbers :: [Int] -> Int
sumTwoSmallestNumbers nums = minimum nums + (minimum (delete (minimum nums) nums))

