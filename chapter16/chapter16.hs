{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

data Expr = Val Int | Add Expr Expr

type Stack = [Int]

type Code = [Op]

data Op = PUSH Int | ADD deriving (Show)

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

exec :: Code -> Stack -> Stack
exec [] s = s
exec (PUSH n : c) s = exec c (n : s)
exec (ADD : c) (m : n : s) = exec c (n + m : s)

-- comp :: Expr -> Code
-- comp (Val n) = [PUSH n]
-- comp (Add x y) = comp x ++ comp y ++ [ADD]

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n : c
comp' (Add x y) c = comp' x (comp' y (ADD : c))

comp :: Expr -> Code
comp e = comp' e []

-- Proof of correctness by induction on e

-- exec (comp' e c) s = exec c (eval e : s)

-- Base case:
-- exec (comp' (Val n) c) s
-- { applying comp' }
-- exec (PUSH n : c) s
-- { applying exec }
-- exec c (n : s)
-- { unapplying eval }
-- exec c (eval (Val n) : s)

-- Inductive case:
-- exec (comp' (Add x y) c) s
-- { applying comp' }
-- exec (comp' x (comp' y (ADD : c))) s
-- { induction hypothesis on x }
-- exec (comp' y (ADD : c)) (eval x : s)
-- { induction hypothesis on y }
-- exec (ADD : c) (eval y : eval x : s)
-- { applying exec }
-- exec c (eval x + eval y : s)
-- { unapplying eval }
-- exec c (eval (Add x y) : s)
