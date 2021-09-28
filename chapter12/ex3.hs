instance Applicative ((->) r) where
  -- pure :: a -> r -> a
  pure = const

  -- <*> :: (r -> a -> b) -> (r -> a) -> (r -> b)
  (<*>) f g x = f x (g x)