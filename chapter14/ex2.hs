instance (Monoid b) => Monoid (a -> b) where
  -- mempty :: a -> b
  mempty = const mempty

  -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
  f `mappend` g = \x -> f x `mappend` g y