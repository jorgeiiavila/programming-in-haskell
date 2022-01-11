filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF f = foldMap (\x -> [x | f x])
