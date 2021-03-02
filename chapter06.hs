-- Chapter 6 - Recursive functions

mult :: Int -> Int -> Int
mult m 0 = 0
mult m n = m + (mult m (n - 1)) 

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x : y : ys
                | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

-- Exercises

-- Exercise 1
fact :: Int -> Int
fact 0 = 1
fact n | n >= 0 = n * fact (n - 1)
       | otherwise = error "Invalid number."

-- Exercise 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n | n >= 0 = n + sumdown (n - 1)
          | otherwise = error "Invalid number."

-- Exercise 3
exp2 :: Int -> Int -> Int
m `exp2` 0 = 1
m `exp2` n = m * (m `exp2` (n - 1))

{- 
    Evaluation of the expression "2 ^ 3":

    2 ^ 3
    = { applying ^ }
    2 * (2 ^ 2)
    = { applying ^ }
    2 * (2 * (2 ^ 1))
    = { applying ^ }
    2 * (2 * (2 * (2 ^ 0)))
    = { applying ^ }
    2 * (2 * (2 * 1))
    = { applying * }
    8
-}

-- Exercise 4
euclid :: Int -> Int -> Int
euclid m n | m == n = m
           | m < n = euclid m (n - m)
           | n < m = euclid (m - n) n

-- Exercise 5

{- 
    a. length [1,2,3]

    length [1,2,3]
    = { applying length }
    1 + (length [2,3])
    = { applying length }
    1 + (1 + (length [3]))
    = { applying length }
    1 + (1 + (1 + (length [])))
    = { applying length }
    1 + (1 + (1 + 0))
    = { applying + }
    3

    b. drop 3 [1,2,3]

    drop 3 [1,2,3]
    = { applying drop }
    drop 2 [2,3]
    = { applying drop }
    drop 1 [3]
    = { applying drop }
    drop 0 []
    = { applying drop }
    []

    c. init [1,2,3]

    init [1,2,3]
    = { applying init }
    1 : (init [2,3])
    = { applying init }
    1 : (2 : (init [3]))
    = { applying init }
    1 : (2 : [])
    = { applying : }
    [1,2]
-}

-- Exercise 6
-- a.
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) | x = and' xs
            | otherwise = False

-- b.
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat xs

-- c.
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate (n - 1) x

-- d.
(!!!) :: [a] -> Int -> a
(!!!) [] _ = error "out of bounds"
(!!!) (x:_) 0 = x
(!!!) (_:xs) n | n >= 0 = xs !!! (n - 1) 
               | otherwise = error "negative index"

-- e.
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs) | n == x = True
               | otherwise = elem' n xs

-- Exercise 7
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- Exercise 8
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge xss ys
    where
        (left, right) = halve xs
        xss = msort left
        ys = msort right

halve :: [a] -> ([a], [a])
halve xs = (xss, ys) 
    where
        half = (length xs) `div` 2
        xss = take half xs
        ys = drop half xs

-- Exercise 9
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

take' :: Int -> [Int] -> [Int]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (_:xs) = last' xs
