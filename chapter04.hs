-- Chapter 4 - Defining functions

-- Exercises

-- Exercise 1
halve :: [a] -> ([a], [a])
halve xs = (a, b) where 
                    half = length xs `div` 2
                    a = take half xs
                    b = drop half xs

-- Exercise 2

-- With head / tail
third :: [a] -> a
third xs = head (tail (tail xs))

-- With indexing !!
third' xs = xs !! 2

-- With pattern matching
third'' (_:(_:(x:_))) = x

-- Exercise 3

-- With conditional expression
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

-- With guarded equations
safetail' xs | null xs = []
             | otherwise = tail xs

-- With pattern matching
safetail'' [] = []
safetail'' xs = tail xs

-- Exercise 4
or :: Bool -> Bool -> Bool
or True _ = True
or _ True = True
or _ _ = False

-- Exercise 5

{-
    Pattern:
    True && True = True
    _    && _    = False
-}
andPatternFormalization :: Bool -> Bool -> Bool
andPatternFormalization x y = if x then if y then True else False
                    else  False

-- Exercise 6
{-
    Pattern:
    True && b = b
    False && _ = False
-}
andPatternFormalization' x y = if x then y else False

-- Exercise 7
{-
    Formalize the following function:
    mult :: Int -> Int -> Int -> Int
    mult x y z = x * y * z
-}

mult :: Int -> Int -> Int -> Int
mult = (\x -> (\y -> (\z -> x * y * z)))

-- Exercise 8
luhnDouble :: Int -> Int
luhnDouble x | x * 2 > 9 = (x * 2) - 9
             | otherwise = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = luhnSum `mod` 10 == 0
                where luhnSum = (luhnDouble w) + x + (luhnDouble y) + z