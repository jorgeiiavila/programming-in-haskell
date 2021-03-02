-- Chapter 2 - First steps

-- Notes
{-
    1.- Identifiers name must start with a lowercase letter.
    2.- Spacing is relevant in Haskell. It is recommended to not use
    tabs, because editors can read them in different ways.

    -- Relevant list functions
    - length
    - head
    - tail
    - init
    - last
    - take
    - drop
-}

-- Exercises

{- 
    1 - Fix the typos

    Typos:
    1.- n was a capital N
    2.- Incorrect indentation of the binded variables
-}
n = a `div` length xs 
    where 
        a = 10 
        xs = [1,2,3,4,5]

-- Exercise 2
altLast xs = head (reverse xs)

-- Exercise 3
initOne xs = take ((length xs) - 1) xs
initTwo xs = reverse (tail (reverse xs))