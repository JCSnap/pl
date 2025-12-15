## Haskell cheatsheet

### List
```haskell
[1..5] -- [1,2,3,4,5]
[1,3..10] -- [1,3,5,7,9]
replicate 4 'a' -- ['a','a','a','a']
cycle [1,2,3]   -- infinite cycle [1,2,3,1,2,3...]
repeat 3 -- infinite [3,3,3....]
iterate (*2) 1 -- infinite [1,2,4....]

[x^2 | x <- [1,2,3,4,5], x > 3] -- [16,25]
-- | reads as "such that", it follows mathematical set notation
[(a,b) | a <- [1,2], b <- [4,5]] -- [(1,4),(1,5),(2,4),(2,5)]
 
take 3 [1,2,3,4] -- [1,2,3]
drop 3 [1,2,3,4] -- [4]
splitAt 3 [1,2,3,4,5] -- ([1,2,3],[4,5])
takeWhile odd [1, 3, 5, 6, 8] -- Output: [1, 3, 5]
dropWhile 

head [1,2,3] -- 1
tail [1,2,3] -- [2,3]

[1,2,3,4,5] !! 0 -- 1 indexed

[1,2] ++ [3,4] -- [1,2,3,4]
1 : [2,3]      -- [1,2,3]

map (+1) [1,2,3]   -- [2,3,4]
map (\x -> x + 1) [1,2,3] -- equivalent as above
filter even [1..10] -- [2,4,6,8,10]
foldl (+) 0 [1,2,3] -- 6
foldr (:) [] [1,2,3] -- [1,2,3]

sum
product
maximum
minimum

elem 3 [1,2,3,4] -- True, elem 5 [1,2,3,4] -> False
notElem 3 [1,2,3,4] -- False

zip [1,2,3] [4,5,6] -- [(1,4),(2,5),(3,6)]
```

### Tuples
```haskell
fst (1,"a")  -- 1
snd (1,"a")  -- "a"
```

### Pattern matching
```haskell
(a:b:rest) = [1,2,3,4]
a -- 1
b -- 2
rest -- [3,4]
-- can be generalised, eg. (x:xs), (a:b:c:rest)

f []     = 0
f (x:xs) = x + f xs

case xs of
  []     -> 0
  (x:_)  -> x

abs' x
  | x >= 0    = x
  | otherwise = -x
  
abs x = if x >= 0 then x else -x
```

### Types
```haskell
a -> b -> c -- means a -> (b -> c) (Right associative)
(+) :: Num a => a -> a -> a -- “Given an `a`, return a function that takes another `a` and returns an `a`”


Int      -- fixed size
Integer  -- arbitrary precision
Float
Double
Bool
Char
String   -- [Char]

[a]
[Int]
[String]
```

### Data
```haskell
data Shape -- can have multipe constructors
  = Circle Double
  | Rect Double Double
  
type UserId = Int -- alias only

data Point = Point Double Double
p = Point 1.0 2.0 -- instantiate
getX (Point x _) = x -- pattern match

data User = User
  { userId :: Int
  , name   :: String
  , age    :: Int
  } deriving (Show, Eq)
u = User { userId = 1, name = "Ada", age = 20 } -- instantiate
name u      -- "Ada"
age u       -- 20
u2 = u { age = age u + 1 } -- update
greet User{name=n} = "Hi " ++ n -- pattern match

c = Circle 3
r = Rect 2 5

area (Circle rad)   = pi * rad * rad
area (Rect w h)     = w * h

data Tree a -- recursive type
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving (Show)

```
### Containers
```haskell
data Maybe a = Nothing | Just a
data Either e a = Left e | Right a
```

### Useful operations
```haskell
print $ sum $ map (+1) [1,2,3] -- print (sum (map (+1) [1,2,3]))

show 1 -- "1" convert to string
print 1 -- print

interact -- “Take a pure function that transforms all input text into output text, and wire it to stdin/stdout.”

square x = x^2
add3 x = x + 3
combined = square . add3 -- compose functions together
combined 3 -- 36

`mod` -- %
/= -- !=
`div` -- //

toUpper
toLower

op = (+) -- assign + to op
op 3 2 -- 5
```
