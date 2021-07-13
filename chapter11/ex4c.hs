{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Char ( isDigit )
import Data.List ( transpose )
import System.IO ( hSetBuffering, stdout, BufferMode(NoBuffering) ) 

size :: Int
size = 3

type Grid = [[Player]]
data Player = O | B | X deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Int -> Grid
empty s = replicate s (replicate s B)

full :: Grid -> Bool 
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
            where
                os = length (filter (== O) ps)
                xs = length (filter (== X) ps)
                ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
            where
                line = all (== p)
                rows = g
                cols = transpose g
                dias = [diag g, diag (map reverse g)]



won :: Grid -> Bool
won g = wins O g || wins X g

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
            where
                beside = foldr1 (zipWith (++))
                bar = replicate 3 "|"

showPlayer :: Player -> [String] 
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

example = [[O, X, O], [X, X, O], [O, X, B]]

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do
                    putStr prompt
                    xs <- getLine
                    if xs /= [] && all isDigit xs then
                        return (read xs)
                    else
                        do
                            putStrLn "ERROR: Invalid number"
                            getNat prompt

tictactoe :: IO ()
tictactoe = run (empty size) O

run :: Grid -> Player -> IO ()
run g p = do
            cls
            goto (1,1)
            putGrid g
            run' g p

cls :: IO ()
cls = putStrLn "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

run' :: Grid -> Player -> IO ()
run' g p | wins O g = putStrLn "Player O wins!\n"
         | wins X g = putStrLn "Player X wins!\n"
         | full g = putStrLn "It's a draw!\n"
         | otherwise = do
                        i <- getNat (prompt p)
                        case move g i p of
                            [] -> do 
                                    putStrLn "ERROR: Invalid move"
                                    run' g p
                            [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

data Tree a = Node a [Tree a] deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

depth :: Int
depth = 9

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g []) | wins O g = Node (g, O) []
                    | wins X g = Node (g, X) []
                    | otherwise = Node (g, B) []
minimax (Node g ts) | turn g == O = Node (g, minimum ps) ts'
                    | turn g == X = Node (g, maximum ps) ts'
                                    where
                                        ts' = map minimax ts
                                        ps = [p | Node (_, p) _ <- ts']

-- Exercise 4a
chooseOrder :: IO String
chooseOrder = do
                print "First or Second? (f | s)"
                p <- getLine
                if p `notElem` ["f", "s"] then do
                    print "Please insert a valid character (f | s)"
                    chooseOrder
                else 
                    return p

-- Exercise 4b
diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..length g - 1]]

putGrid :: Grid -> IO ()
putGrid g = (putStrLn . unlines . concat . interleave bar . map showRow) g
            where bar = [replicate ((length g * 4) - 1) '-']

valid :: Grid -> Int -> Bool 
valid g i = 0 <= i && i < length g ^ 2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i then [chop (length g) (xs ++ [p] ++ ys)] else []
                where
                    (xs,B:ys) = splitAt i (concat g)

moves :: Grid -> Player -> [Grid]
moves g p | won g = []
          | full g = []
          | otherwise = concat [move g i p | i <- [0..((length g ^ 2) - 1)]]

-- Exercise 4c
main :: IO ()
main = do
        hSetBuffering stdout NoBuffering
        order <- chooseOrder
        s <- getNat "Enter board size: "
        let p = if order == "f" then O else X
        let g = empty s
        let gt = prune depth (gametree g p)
        play g p gt

bestmove :: Grid -> Player -> Tree Grid -> Grid
bestmove g p gt = head [g' | Node (g', p') _ <- ts, p' == best]
                where
                    Node (_, best) ts = minimax gt

play :: Grid -> Player -> Tree Grid -> IO ()
play g p gt = do
            cls
            goto (1, 1)
            putGrid g
            play' g p gt

play' :: Grid -> Player -> Tree Grid -> IO ()
play' g p gt | wins O g = putStrLn "Player 0 wins!\n"
             | wins X g = putStrLn "Player X wins!\n"
             | full g   = putStrLn "It's a draw!\n"
             | p == O = do 
                        i <- getNat (prompt p)
                        case move g i p of
                            [] -> do
                                    putStrLn "ERROR: Invalid move"
                                    play' g p gt
                            [g'] -> play g' (next p) (getNextGametreeChild gt g')
          | p == X = do
                        putStr "Player X is thinking... "
                        let newgrid = bestmove g p gt
                        (play $! newgrid) (next p) (getNextGametreeChild gt newgrid)


getNextGametreeChild :: Tree Grid -> Grid -> Tree Grid
getNextGametreeChild (Node _ xs) newg = head (filter (\(Node g _) -> g == newg) xs)

