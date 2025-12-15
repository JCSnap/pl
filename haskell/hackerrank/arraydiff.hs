-- If a = [1, 2] and b = [1], the result should be [2].
-- If a = [1, 2, 2, 2, 3] and b = [2], the result should be [1, 3].

difference :: Eq a => [a] -> [a] -> [a]
difference a b = [aElem | aElem <- a, aElem notElem b]


