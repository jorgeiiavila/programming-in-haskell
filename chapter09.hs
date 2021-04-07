-- Chapter 9 - The countdown problem

import Data.List

data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Exp = "^"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0
valid Exp x y = x /= 0 || y > 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n) = show n
    show (App o l r) = brak l ++ show o ++ brak r
                        where
                            brak (Val n) = show n
                            brak e = "(" ++ show e ++ ")" 

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]


subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
                where
                    yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms = foldr (concatMap . interleave) [[]]

choices :: [a] -> [[a]]
choices = concatMap perms . subs

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

-- Optimized version
type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns,
                    lx <- results ls,
                    ry <- results rs,
                    res <- combine' lx ry]

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y 
valid' Div x y = y /= 1 && x `mod` y == 0
valid' Exp x y = x /= 0 || y > 0
                    
combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid' o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e, m) <- results ns', m == n]

exampleOne = solutions' [1,3,7,10,25,50] 765

{- 
    To compile the program run the following command:
    ghc -O2 chapter09.hs
-}

-- Exercices

-- Exercise 1
choices' :: [a] -> [[a]]
choices' ns = [choice | n <- subs ns, 
                        choice <- perms n]

-- Exercise 2
removeOne :: Eq a => a -> [a] -> [a]
removeOne _ [] = []
removeOne n (x:xs) | n == x = xs
                        | otherwise = x : removeOne n xs

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (_:_) [] = False
isChoice xs (y:ys) = isChoice (removeOne y xs) ys

-- Exercise 3
{-
    The function will loop forever for lists with more than one 
    element because the first element of the last tuple of the 
    list returned by the split function will always be the original 
    list.
-}

-- Exercise 4
allExprs = concatMap exprs (choices [1,3,7,10,25,50])
-- There are 33,665,406 possible expressions
validExprs = filter (not . null) (map eval allExprs) 
-- There are 4672540 valid expressions

-- Exercise 5
valid'' :: Op -> Int -> Int -> Bool
valid'' Add _ _ = True
valid'' Sub x y = True
valid'' Mul _ _ = True
valid'' Div x y = y /= 0 && x `mod` y == 0

eval' :: Expr -> [Int]
eval' (Val n) = [n | n > 0]
eval' (App o l r) = [apply o x y | x <- eval' l, y <- eval' r, valid'' o x y]

validExprs' = filter (not . null) (map eval' allExprs)
-- There are 10839369 valid expressions

-- Exercise 6

-- b. Produce nearest solution
nearestSolution :: Int -> [Expr] -> [Expr]
nearestSolution _ [] = []
nearestSolution x (y:ys) = nearestSolution' x ys y

nearestSolution' :: Int -> [Expr] -> Expr -> [Expr]
nearestSolution' _ [] n = [n]
nearestSolution' x (y:ys) n | abs (x - yValue) < abs (x - nValue) = nearestSolution' x ys y
                            | otherwise  = nearestSolution' x ys n
                                where
                                    [yValue] = eval y
                                    [nValue] = eval n

first (x, _) = x

solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns n | null exactResults = nearestSolution n allResults
                 | otherwise = exactResults
                    where 
                        exactResults = map first 
                                            (filter 
                                                (\(_, m) -> m == n) 
                                                (concatMap results (choices ns))
                                            )
                        allResults = map first (concatMap results (choices ns))


-- c. Sort by suitable measure of simplicity
-- Here the measure is to sort in ascending order the expressions
-- by the number of operations they perform.

countOps :: Expr -> Int
countOps (Val _) = 0
countOps (App o l r) = 1 + countOps l + countOps r

sortedSolutions :: [Int] -> Int -> [Expr]
sortedSolutions xs x = sortBy ascSort (solutions xs x)

ascSort :: Expr -> Expr -> Ordering
ascSort x y = compare (countOps x) (countOps y)
