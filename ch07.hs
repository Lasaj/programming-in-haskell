import Data.Char

-- 1
q1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
q1 f p xs = map f $ filter p xs

-- 2
--a
all' :: (a -> Bool) -> [a] -> Bool
all' f xs = and $ map f xs

--b
any' :: (a -> Bool) -> [a] -> Bool
any' f xs = or $ map f xs

--c
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x       = x : takeWhile' f xs
    | otherwise = []

--d
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs)
    | f x       = dropWhile' f xs
    | otherwise = x:xs

-- 3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []
map'' f = foldr ((:) . f) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x xs -> if f x then x:xs else xs) []

-- 4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y) 0

-- 5
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x,y)

uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f (x,y) = f x y

-- 6
type Bit = Int

unfold p h t x
    | p x       = []
    | otherwise = h x : unfold p h t (t x)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

mapUnfold :: (a -> b) -> [a] -> [b]
mapUnfold f = unfold null (f . head) tail

iterateUnfold :: (a -> a) -> a -> [a]
iterateUnfold = unfold (const False) id

-- 9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ []     = []
altMap f g (x:xs) = f x : altMap g f xs

-- 0
luhnDouble :: Int -> Int
luhnDouble n 
    | n * 2 > 9 = n * 2 - 9
    | otherwise = n * 2

luhn :: [Int] -> Bool
luhn xs = sum (altMap luhnDouble id xs) `mod` 10 == 0