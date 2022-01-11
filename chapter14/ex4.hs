data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  -- fold :: Monoid a => Tree a -> a
  --   fold Leaf = mempty
  --   fold (Node l x r) = fold l `mappend` a `mappend` fold r
  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap _ Leaf = mempty
  foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r

  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ v Leaf = v
  foldr f v (Node l x r) = foldr f (foldr f (f x v) r) l

  -- foldl :: (a -> b -> a) -> a -> Tree b -> a
  foldl _ v Leaf = v
  foldl f v (Node l x r) = foldl f (foldl f (f v x) l) r

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Leaf = pure Leaf
  traverse f (Node l x r) = pure Node <*> traverse f l <*> f x <*> traverse f r