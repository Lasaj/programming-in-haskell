
-- 3
prod :: [Int] -> Int
prod = foldr (*) 1

-- 4
qsort :: (Ord a) => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
    where
        smaller = [a | a <- xs, a <= x]
        larger  = [b | b <- xs, b > x]

-- 5
-- will drop duplicates