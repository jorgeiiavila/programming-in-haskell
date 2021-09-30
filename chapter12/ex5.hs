-- Applicative Laws

-- pure :: Applicative f => a -> f a
-- <*> :: Applicative f => f (a -> b) -> f a -> f b

-- 1.-
-- pure id <*> x = x
-- id :: a -> a
-- x :: f a

-- 2.-
-- pure (g x) = pure g <*> pure x
-- g :: a -> b
-- x :: a

-- 3.-
-- x <*> pure y = pure (\g -> g y) <*> x
-- x :: f (a -> b)
-- y :: a
-- g :: a -> b

-- 4.-
-- x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- x :: f (b -> c)
-- y :: f (a -> b)
-- z :: f a