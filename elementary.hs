-- All functions will be ended with '.

-- head'
head' :: [a] -> a
head' [] = error "head': empty list"
head' (x:xs) = x

-- tail'
tail' :: [a] -> [a]
tail' [] = error "tail': empty list"
tail' (x:xs) = xs

-- last'
last' :: [a] -> a
last' [] = error "last': empty list"
last' [x] = x
last' (x:xs) = last' xs

-- init'
init' :: [a] -> [a]
init' [] = error "init': empty list"
init' [x] = []
init' (x:xs) = x : init' xs

-- fst'
fst' :: (a, b) -> a
fst' (a, _) = a

-- snd'
snd' :: (a, b) -> b
snd' (_, b) = b

-- min'
min' :: (Ord a) => a -> a -> a
min' x y
	| x > y = y
	| otherwise = x

-- max'
max' :: (Ord a) => a -> a -> a
max' x y
	| x > y = x
	| otherwise = y

-- minimum'
minimum' :: (Ord a) => [a] -> a
minimum' [] = error "minimum': empty list"
minimum' [x] = x
minimum' (x:xs) = min x (minimum' xs)

-- maximum'
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum': empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- take'
take' :: Int -> [a] -> [a]
take' n _
	| n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

-- reverse'
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- replicate'
replicate' :: Int -> a -> [a]
replicate' n x
	| n <= 0 = []
	| otherwise = x : replicate' (n-1) x

-- repeat'
repeat' :: a -> [a]
repeat' x = x : repeat' x

-- and'
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs)
	| x = and' xs
	| otherwise = False

-- or'
or' :: [Bool] -> Bool
or' [] = False
or' (x:xs)
	| x = True
	| otherwise = or' xs

-- not'
not' :: Bool -> Bool
not' x
	| x = False
	| otherwise = True

-- elem'
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs)
	| n == x = True
	| otherwise = elem' n xs

-- id'
id' :: a -> a
id' x = x

-- length'
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- null'
null' :: [a] -> Bool
null' [] = True
null' x = False

-- product'
product' :: (Num a) => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

-- sum'
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- odd'
odd' :: (Integral a) => a -> Bool
odd' x
	| mod x 2 == 0 = False
	| otherwise = True

-- even'
even' :: (Integral a) => a -> Bool
even' x
	| mod x 2 == 0 = True
	| otherwise = False

-- gcd'
gcd' :: (Integral a) => a -> a -> a
gcd' x 0 = x
gcd' 0 y = y
gcd' x y = abs (gcd' y (x `rem'` y))

-- lcm'
lcm' :: (Integral a) => a -> a -> a
lcm' _ 0 = 0
lcm' 0 _ = 0
lcm' x y = abs (y * (x `quot'` (gcd' x y)))

-- rem'
rem' :: (Integral a) => a -> a -> a
rem' _ 0 = error "rem': divide by zero"
rem' 0 _ = 0
rem' x y = x - (y * (x `quot'` y))

-- quot'
quot' :: (Integral a) => a -> a -> a
quot' _ 0 = error "quot': divide by zero"
quot' 0 _ = 0
quot' x y
	| and [x > 0, y < 0, ax < ay] = 0
	| and [x < 0, y > 0, ax < ay] = 0
	| and [x > 0, y < 0, ax >= ay] = (quot' (x + y) y) - 1
	| and [x < 0, y > 0, ax >= ay] = (quot' (x + y) y) - 1
	| otherwise = 1 + quot' (ax - ay) ay
	where (ax, ay) = (abs x, abs y)

-- map'
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- mod'
mod' :: (Integral a) => a -> a -> a
mod' _ 0 = error "mod': divide by zero"
mod' 0 _ = 0
mod' x y
	| (x `rem'` y) < 0 = y + x `rem'` y
	| otherwise = x `rem'` y

-- zip'
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- zip3'
zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3' xs ys zs

-- flip'
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

-- filter'
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
	| f x = x : filter' f xs
	| otherwise = filter' f xs

