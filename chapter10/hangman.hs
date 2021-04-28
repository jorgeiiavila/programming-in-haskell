-- Hangman game

import System.IO

hangman :: IO ()
hangman = do
            putStrLn "Think of a word:"
            word <- sgetLine
            putStrLn "Try to guess it:"
            playHangman word

sgetLine :: IO String
sgetLine = do 
            x <- getCh
            if x == '\n' then
                do
                    putChar x
                    return []
            else
                do
                    putChar '-' 
                    xs <- sgetLine
                    return (x:xs)

getCh :: IO Char
getCh = do
            hSetEcho stdin False
            x <- getChar
            hSetEcho stdin True
            return x

playHangman :: String -> IO ()
playHangman word = do
                putStr "? "
                guess <- getLine
                if guess == word then
                    putStrLn "You got it!!"
                else
                    do 
                        putStrLn (match word guess)
                        playHangman word

match :: String -> String -> String
match xs ys = [if x `elem` ys then x else '-' | x <- xs]