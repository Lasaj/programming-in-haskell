-- 1
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

t :: Tree Int
t = Node (Node (Leaf) 3 (Leaf)) 5 (Node (Leaf) 7 (Leaf))

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf)       = Leaf
    fmap g (Node l y r) = Node (fmap g l) (g y) (fmap g r)