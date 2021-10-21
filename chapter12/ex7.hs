data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving (Show)

instance Functor Expr where
  -- fmap :: (a -> b) -> Expr a -> Expr b
  fmap f (Var x) = Var (f x)
  fmap _ (Val x) = Val x
  fmap f (Add l r) = Add (fmap f l) (fmap f r)

instance Applicative Expr where
  -- pure :: a -> Expr a
  pure = Var

  -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  (Val x) <*> _ = Val x
  _ <*> (Val x) = Val x
  (Var f) <*> (Var x) = Var (f x)
  (Var f) <*> (Add l r) = Add (fmap f l) (fmap f r)
  (Add l r) <*> x = Add (l <*> x) (r <*> x)

instance Monad Expr where
  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  Val x >>= _ = Val x
  Var x >>= f = f x
  Add l r >>= f = Add (l >>= f) (r >>= f)

-- Examples
ex1 = Var 10 >>= (\x -> Var (x * 10))

-- Output: Var 100

ex2 = Add (Var 10) (Var 20) >>= (\x -> Var (x ^ 2))

-- Output: Add (Var 100) (Var 400)