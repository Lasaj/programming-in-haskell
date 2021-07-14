-- 1
halve' :: [a] -> ([a], [a])
halve' xs = splitAt n xs
    where n = length xs `div` 2

-- 2
-- a
third :: [a] -> a
third = head . tail . tail
-- b
third' xs = xs!!2
-- c
third'' (x:y:z:zs) = z 

-- 3
-- a
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs
-- b
safetail' xs 
    | null xs = []
    | otherwise = tail xs
-- c
safetail'' []     = []
safetail'' (x:xs) = xs

-- 4
or :: Bool -> Bool -> Bool
or True True   = True
or True False  = True
or False True  = True
or False False = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _         = True

or'' :: Bool -> Bool -> Bool
or'' False b = b
or'' True _  = True

or''' :: Bool -> Bool -> Bool
or''' b c
    | b == c    = b
    | otherwise = True

-- 5
and x y = if x == True then if y == True then True else False else False

-- 6
and' x y = if x == True then y else False

-- 8
luhnDouble :: Int -> Int
luhnDouble n 
    | n * 2 > 9 = n * 2 - 9
    | otherwise = n * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = sum [luhnDouble a, b, luhnDouble c, d] `mod` 10 == 0