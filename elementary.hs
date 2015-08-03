-- All functions will be ended with '.
-- Some Lisp-style elementary functions will appear, of course without
-- side-effect.

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
init' (x:xs) = x:init' xs

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

