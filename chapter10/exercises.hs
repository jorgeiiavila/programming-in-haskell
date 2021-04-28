import System.IO

-- Exercise 1
putStr' :: String -> IO ()
putStr' s = sequence_ [x | x <- map putChar s]

-- Exercise 2
type Board = [Int]

putBoard :: Board -> IO ()
putBoard b = putBoardHelper b 1
                    
putBoardHelper :: Board -> Int -> IO ()
putBoardHelper [] _ = return ()
putBoardHelper (x:xs) row = do
                        putRow row x
                        putBoardHelper xs (row + 1)

putRow :: Int -> Int -> IO ()
putRow row n = do
                putStr (show row)
                putStr ": "
                putStrLn (concat (replicate n "*")) 

-- Exercise 3
putBoard' :: Board -> IO ()
putBoard' b = sequence_ [putRow row n | (row, n) <- zip [1..l] b]
                where l = length b

-- Exercise 4
adder :: IO ()
adder = do
            putStr "How many numbers? "
            n <- getLine
            res <- adderHelper 0 (read n :: Int)
            putStrLn  ("The total is " ++ show res)

adderHelper :: Int -> Int -> IO Int
adderHelper total 0 = return total
adderHelper total n = do 
                        x <- getLine
                        adderHelper (total + (read x :: Int)) (n - 1)

-- Exercise 5
adder' :: IO ()
adder' = do
            putStr "How many numbers? "
            n <- getLine
            nums <- sequence (replicate (read n :: Int) getLine)
            putStrLn ("The total is " ++ show (sum (map (\x -> read x :: Int) nums)))

-- Exercise 6
getCh :: IO Char
getCh = do
            hSetEcho stdin False
            x <- getChar
            hSetEcho stdin True
            return x

readLine :: IO String
readLine = readLineHelper ""

readLineHelper :: String -> IO String
readLineHelper s = do
                        c <- getCh
                        if c == '\DEL' then
                            do 
                                putChar '\b'
                                readLineHelper (if not (null s) then init s else "")
                        else if c == '\n' then
                            do 
                                putChar c
                                return s
                        else
                            do
                                putChar c
                                readLineHelper (s ++ [c])
            
