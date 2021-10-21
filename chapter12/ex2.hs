newtype F a b = F (a -> b)

instance Functor (F a) where
  -- fmap :: (a -> b) -> F (a1 -> a) -> F (a1 -> b)
  fmap f (F x) = F (f . x)
