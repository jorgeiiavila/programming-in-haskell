instance Foldable Maybe where
  --   fold :: Monoid a => Maybe a -> a
  fold Nothing = mempty
  fold (Just a) = a

  --   foldMap :: Monoid b => (a -> b) -> Maybe a -> b
  foldMap _ Nothing = mempty
  foldMap f (Just x) = f x

  --   foldr :: (a -> b -> b) -> b -> Maybe a -> b
  foldr _ _ Nothing = mempty
  foldr f b (Just a) = f a b

  --   foldl :: (a -> b -> a) -> a -> Maybe b -> a
  foldl _ _ Nothing = Nothing
  foldl f a (Just b) = f a b

instance Traversable Maybe where
  -- traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
  traverse _ Nothing = pure Nothing
  traverse f (Just a) = Just <*> f a
