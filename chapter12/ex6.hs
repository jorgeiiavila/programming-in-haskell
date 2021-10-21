instance Monad ((->) a) where
  -- return :: a -> (b -> a)
  return = const

  -- >>= :: (a -> b) -> (b -> (a -> c)) -> (a -> c)
  (>>=) f g x = g (f x) x