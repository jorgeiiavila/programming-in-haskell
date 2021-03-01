import Data.Char

-- Chapter 5 - List Comprehension

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

-- Exercises

-- Problem 1
squaresSum :: Int
squaresSum = sum [x * x | x <- [1..100]]

-- Problem 2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- Problem 3
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- Problem 4
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

-- Problem 5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- Problem 6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [2..n], sum (factors x) - x == x]

-- Problem 7
problemSeven = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]

-- Problem 8
positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = [i | i <- find x (zip xs [0..])]

-- Problem 9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- Problem 10 - Caesar Cipher
let2Int :: Char -> Int
let2Int c = ord c - ord 'a'

int2let :: Int -> Char
int2let i = chr (i + ord 'a')

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2Int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
            where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 
    12.7, 2.2, 2.0, 6.1, 
    7.0, 0.2, 0.8, 4.0, 
    2.4, 6.7, 7.5, 1.9, 
    0.1, 6.0, 6.3, 9.0, 
    2.8, 1.0, 2.4, 0.2, 
    2.0, 0.1]

crack :: String -> String
crack xs = encode (-factor) xs
    where 
        factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs