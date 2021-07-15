-- 1
data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add m Zero = m
add Zero n = n
add m (Succ n) = add (Succ m) n

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult Zero n = Zero
mult m (Succ n) = add m $ mult m n 

-- 2
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = case compare x y of
    LT -> occurs x l
    EQ -> True
    GT -> occurs x r

-- 3
data BinTree a = BinLeaf a | BinNode (BinTree a) (BinTree a) deriving Show

t2 :: BinTree Int
t2 = BinNode (BinNode (BinLeaf 1) (BinNode (BinLeaf 4) (BinLeaf 5))) (BinNode (BinLeaf 6) (BinLeaf 9))

t3 :: BinTree Int
t3 = BinNode (BinNode (BinLeaf 1) (BinLeaf 5)) (BinNode (BinLeaf 6) (BinLeaf 9))

leaves :: BinTree a -> Int 
leaves (BinLeaf _)   = 1
leaves (BinNode l r) = leaves l + leaves r

balanced :: BinTree a -> Bool 
balanced (BinLeaf _)   = True
balanced (BinNode l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

-- 4
halve :: [a] -> ([a], [a])
halve xs = splitAt n xs
    where n = length xs `div` 2

balance :: [a] -> BinTree a
balance [x] = BinLeaf x
balance xs  = BinNode (balance ys) (balance zs)
    where (ys, zs) = halve xs

-- 5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x)   = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- 6
eval :: Expr -> Int
eval = folde id (+)

-- 7.
--instance Eq a => Eq (Maybe a) where
--  Just x  == Just y     = x == y
--  Nothing == Nothing    = True
--  _       == _          = False

--instance Eq a => Eq [a] where
--  [] == []         = True
--  (x:xs) == (y:ys) = x == y && xs == ys
--  _ == _           = False
