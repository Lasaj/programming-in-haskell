-- 1
q1 = sum [x^2 | x <- [1..100]]

-- 2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [1..m], y <- [1..n]]

-- 3
square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x /=y]

-- 4
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

-- 5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [y..n], x^2 + y^2 == z^2]

-- 6
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], (sum . factors) x == x]
    where factors x = [y | y <- [1..x-1], x `mod` y == 0]

-- 7
q7 = concat [[(x,y) | y <- [3, 4]] | x <- [1,2]]

-- 8
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x $ zip xs [0..]

-- 9
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x, y) <- zip xs ys]
