import Data.Char (digitToInt, isDigit)

fmap2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
fmap2 g x y = pure g <*> x <*> y

x = pure (+ 1) <*> Just 1

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

y = pure (+) <*> [1, 2, 3] <*> [4, 5, 6]

getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n - 1)

-- sequenceA - Uses the idea of applicatives to chain actions
-- getChars can be redefined as follows
getChars' :: Int -> IO String
getChars' n = sequenceA (replicate n getChar)

-- Aplicative Laws
-- 1.- pure id <*> x   = x
-- 2.- pure (g x)      = pure g <*> pure x
-- 3.- x <*> pure y    = pure (\g -> g y) <*> x
-- 4.- x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

-- 1.- The pure function preserves the identity function
-- 2.- Function application is preserved
-- 3.- When an effectful function is applied to a pure argument.
--     the evaluation order does not matter.
-- 4.- The operator <*> is assosiative

-- 12.3 - Monads

data Expr = Val Int | Div Expr Expr

-- This eval definition does not consider the divide by zero
-- edge case, so it will produce an error in that case

-- eval :: Expr -> Int
-- eval (Val n) = n
-- eval (Div x y) = eval x `div` eval y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

-- This version is better, as it handles the error. It is
-- quire verbose though

-- eval :: Expr -> Maybe Int
-- eval (Val n) = Just n
-- eval (Div x y) = case eval x of
--   Nothing -> Nothing
--   (Just n) -> case eval y of
--     Nothing -> Nothing
--     (Just m) -> safediv n m

-- This definition is not type correct, as it is required
-- for safediv to be of type Int -> Int -> Int. With this
-- it becomes clear that eval does not fit the applicative
-- style, as safediv is not a pure function.

-- eval :: Expr -> Maybe Int
-- eval (Val n) = pure n
-- eval (Div x y) = pure safediv <*> eval x <*> eval y

-- eval :: Expr -> Maybe Int
-- eval (Val n) = Just n
-- eval (Div x y) = eval x >>= (\n -> eval y >>= (\m -> safediv n m))

-- Final version using the do notation
eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = do
  n <- eval x
  m <- eval y
  safediv n m

-- The State Monad

type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = S (\s -> let (x, s') = app st s in (g x, s'))

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x, s))

  -- <*> :: ST (a -> b) -> ST a -> ST b
  stf <*> stx =
    S
      ( \s ->
          let (f, s') = app stf s
              (x, s'') = app stx s'
           in (f x, s'')
      )

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f =
    S
      ( \s ->
          let (x, s') = app st s in app (f x) s'
      )

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n + 1)
rlabel (Node l r) n = (Node l' r', n'')
  where
    (l', n') = rlabel l n
    (r', n'') = rlabel r n'

fresh :: ST Int
fresh = S (\n -> (n, n + 1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) =
  do
    n <- fresh
    return (Leaf n)
mlabel (Node l r) = do
  l' <- mlabel l
  r' <- mlabel r
  return (Node l' r')

-- Generic Functions
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x : xs) = do
  y <- f x
  ys <- mapM' f xs
  return (y : ys)

conv :: Char -> Maybe Int
conv c
  | isDigit c = Just (digitToInt c)
  | otherwise = Nothing

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' p (x : xs) = do
  b <- p x
  ys <- filterM' p xs
  return (if b then x : ys else ys)

join' :: Monad m => m (m a) -> m a
join' mmx = do
  mx <- mmx
  x <- mx
  return x
