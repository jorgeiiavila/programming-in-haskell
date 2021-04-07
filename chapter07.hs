-- Chapter 7 - Higher-order functions

import Data.Char
import Data.List

-- Examples

-- 1. Binary string transmitter (Modified example below in exercise 7)

-- 2. Voting algorithms

-- a. First past the post
votes :: [String] 
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

-- b. Alternative vote
ballots ::   [[String]] 
ballots = [["Red", "Green"], 
            ["Blue"], 
            ["Green", "Red", "Blue"], 
            ["Blue", "Green", "Red"], 
            ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
                [c] -> c
                (c:cs) -> winner' (elim c bs)

-- Exercises

-- Exercise 1
exercise1 :: (a -> Bool) -> (a -> b) -> [a] -> [b]
exercise1 p f = map f . filter p

-- Exercise 2
-- a.
all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' f (x:xs) | f x = all' f xs
              | otherwise = False

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (x:xs) | f x = True
              | otherwise = any' f xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) | f x = x : takeWhile' f xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs) | f x = dropWhile f xs
                    | otherwise = x:xs

-- Exercise 3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr g []
        where g x xs | f x = x : xs
                     | otherwise = xs

-- Exercise 4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y) 0

-- Exercise 5
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y) 

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y 

-- Exercise 6
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

chop8' :: [a] -> [[a]]
chop8' = unfold null (take 8) (drop 8)

mapf :: (a -> b) -> [a] -> [b]
mapf f = unfold null (f . head) tail

iterate'' :: (a -> a) -> a -> [a]
iterate'' = unfold (const False) id

-- Exercise 7
type Bit = Int

bit2intalt :: [Bit] -> Int
bit2intalt bits = sum [w * b | (w, b) <- zip weights bits]
        where weights = iterate (*2) 1

bit2int :: [Bit] -> Int
bit2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2) 

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

addParityBit :: [Bit] -> [Bit]
addParityBit bits | odd (count 1 bits) = bits ++ [1]
                  | otherwise = bits ++ [0]

encode :: String -> [Bit]
encode = concatMap (addParityBit . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits) 

validBinary :: [Bit] -> Bool
validBinary bits | lastBit == 1 = odd (count 1 (init bits))
                 | lastBit == 0 = even (count 1 (init bits))
                 | otherwise = False
                        where lastBit = last bits

decode :: [Bit] -> String
decode bits | all validBinary bitsMatrix = map (chr . bit2int . init) bitsMatrix
            | otherwise = error "Invalid transmission."
                where bitsMatrix = chop9 bits

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- Exercise 8

faultyChannel :: [Bit] -> [Bit]
faultyChannel (_:xs) = xs

faultyTransmit :: String -> String
faultyTransmit = decode . faultyChannel . encode

-- Exercise 9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f _ [x] = [f x]
altMap f g (x:(y:ys)) = [f x , g y] ++ altMap f g ys

-- Exercise 10
luhnDouble :: Int -> Int
luhnDouble x | x * 2 > 9 = (x * 2) - 9
             | otherwise = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = sum luhnList `mod` 10 == 0
                where luhnList = altMap luhnDouble id [w, x, y, z]
