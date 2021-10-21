type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) = st

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = st >>= \x -> pure (g x)

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x, s))

  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = stf >>= \x -> stx >>= (pure . x)

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f =
    S
      ( \s ->
          let (x, s') = app st s in app (f x) s'
      )
