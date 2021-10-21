newtype F a b = F (a -> b)

instance Functor (F z) where
  -- fmap :: (a -> b) -> F (z -> a) -> F (z -> b)
  fmap f (F x) = F (f . x)

instance Applicative (F z) where
  -- pure :: a -> F z a
  pure x = F (const x)

  -- <*> :: F z (a -> b) -> F z a -> F z b
  (<*>) (F f) (F g) = F (\x -> f x (g x))

(F x) = pure (+ 10)

(F y) = (+ 10) <$> F (+ 100)

main = do
  print (x 20 30)
  print (y 50)
