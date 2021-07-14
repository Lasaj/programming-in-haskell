-- 1
fac :: Int -> Int
fac 0 = 1
fac n
    | n >= 0    = n * fac (n-1)
    | otherwise = 1

-- 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- 3
exp' :: Int -> Int -> Int
exp' _ 0 = 1
exp' n m = n * exp' n (m-1)

-- 4
euclid :: Int -> Int -> Int
euclid n m
    | n == m    = n
    | n > m     = euclid m (n-m)
    | otherwise = euclid n (m-n)

-- 6
--a
and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and' xs

--b
concat' :: [[a]] -> [a]
concat' []     = []
concat' (xs:xss) = xs ++ concat' xss

--c
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

--d
index' :: [a] -> Int -> a
index' xs 0 = head xs
index' xs n = index' xs (n-1)

--e
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs)
    | y == x    = True
    | otherwise = elem' y xs

-- 7
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- 8
halve :: [a] -> ([a], [a])
halve xs = splitAt n xs
    where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = uncurry merge $ fmap msort $ halve xs