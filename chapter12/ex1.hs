-- Exercise 1
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

tree = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))

squaredTree = fmap (^ 2) tree
