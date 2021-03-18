-- Chapter 8 - Declaring types and classes

-- Exercises
data Nat = Zero | Succ Nat deriving Show

data Expr = Val Int | Add Expr Expr | Mult Expr Expr

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

-- Inefficient Add that converts both Nat before performing the Sum
-- add :: Nat -> Nat -> Nat
-- add m n = int2nat (nat2int m + nat2int n)

-- Better implementation that does not require the convertions
add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

-- Problem 1
mult :: Nat -> Nat -> Nat
mult _ Zero = Zero
mult n (Succ y) = add n (mult n y)   

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

t1 :: Tree Int 
t1 = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

t2 :: Tree Int
t2 = Node (Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Leaf 5)) 6 (Leaf 7)

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r


{-
    Problem 2
    This solution is more efficient than the previous occurs function
    because this one takes advantage of the search trees property where every
    number at the left subtree is smaller than or equal to the root, and  every
    number at the right subtree is greater than the root. So, when using this 
    property, at each iteration of the recursion half of the tree is discarded, 
    giving a complexity of O(log N), while the previous occurs is O(N)
-}
occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y) = x == y
occurs' x (Node l y r) | x <= y = occurs x l
                       | otherwise = occurs x r

data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) deriving Show

t3 = Node' 
        (Node' 
            (Node' 
                (Leaf' 1) (Leaf' 2)) 
            (Node'
                (Leaf' 3) (Leaf' 4))) 
        (Node' 
            (Node' 
                (Node' 
                    (Leaf' 5) (Leaf' 6)) 
                (Node' 
                    (Leaf' 7) (Leaf' 8))) 
            (Leaf' 9))

-- Problem 3
balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) = abs (leavesInTree l - leavesInTree r) <= 1 && 
                            balanced l && balanced r

leavesInTree :: Tree' a -> Int
leavesInTree (Leaf' _) = 1
leavesInTree (Node' l r) = leavesInTree l + leavesInTree r

-- Problem 4
balance :: [a] -> Tree' a
balance [] = error "Empty list"
balance [x] = Leaf' x
balance xs = Node' (balance l) (balance r)
                where
                    (l, r) = split xs

split :: [a] -> ([a], [a])
split xs = (l, r)
            where
                size = length xs
                l = take (size `div` 2) xs
                r = drop (size `div` 2) xs

-- Problem 5
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x) =  f x
folde f g (Add x y) = g (folde f g x) (folde f g y)
folde f g (Mult x y) = g (folde f g x) (folde f g y)

-- Problem 6
eval'' :: Expr -> Int
eval'' (Add x y) = folde id (+) (Add x y)
eval'' (Mult x y) = folde id (*) (Mult x y)

size' :: Expr -> Int
size' (Val _) = 1
size' (Add l r) = size' l + size' r
size' (Mult l r) = size' l + size' r

-- Problem 7
data Maybe' a = Nothing' | Just' a 

instance Eq a => Eq (Maybe' a) where
    (Just' x) == (Just' y) = x == y
    Nothing' == Nothing' = True

-- Problem 8
data Prop = Const Bool 
    | Var Char
    | Not Prop
    | And Prop Prop
    | Imply Prop Prop
    | Or Prop Prop
    | Eqv Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

p5 :: Prop
p5 = Or (Var 'a') (Var 'b')

p6 :: Prop
p6 = Or p2 p4

p7 :: Prop
p7 = Eqv p1 p3

p8 :: Prop
p8 = Eqv p2 p4

type Assoc k v = [(k, v)]

type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Eqv p q) = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Eqv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
      where bss = bools (n - 1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
      where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- Exercise 9
value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y
value (Mult x y) = value x * value y

data Op = EVALADD Expr | EVALMULT Expr | ADD Int | MULT Int
type Cont = [Op]

eval' :: Expr -> Cont -> Int
eval' (Val n) c = exec c n
eval' (Add x y) c = eval' x (EVALADD y : c)
eval' (Mult x y) c = eval' x (EVALMULT y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVALADD y : c) n = eval' y (ADD n : c)
exec (EVALMULT y : c) n = eval' y (MULT n : c)
exec (ADD n : c) m = exec c (n + m)
exec (MULT n : c) m = exec c (n * m)

value' :: Expr -> Int
value' e = eval' e []

expr :: Expr
expr = Mult (Add (Val 3) (Val 7)) (Val 10)
