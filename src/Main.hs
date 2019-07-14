module Main where

-- reader: number of letters to match at tail and head
-- writer: term history
-- state:  game state

import Control.Monad.RWS.Strict
import Data.Foldable            (traverse_)
import Data.List                (intersperse)
import Data.List.Extra          (trim)
import Text.Read                (readMaybe)

main :: IO ()
main = do
    n <- go
    (s,w) <- execRWST play (Env {matchLength=n}) (GameState {prev="", gameOver=False})
    showHistory n w
    where
        go = do
            putStrLn "How many letters should match? [default: 1] "
            n <- getLine
            if length (trim n) == 0  -- allow default value
            then return 1
            else case readMaybe n :: Maybe Int of
                Nothing -> putStrLn "Please enter a non-zero whole number." >> go
                Just n -> return n

play :: RWST Env [String] GameState IO ()
play = do
    s <- get
    if gameOver s
    then showGameOver
    else do
        listen prompt >>= (\(a,w) -> tell [a])
        play

prompt :: RWST Env [String] GameState IO String
prompt = do
    s <- get
    env <- ask
    liftIO $ putStrLn "\nEnter a word: "
    entry <- liftIO $ getLine
    if length (trim entry) == 0
    then prompt  -- prompt until valid entry
    else 
        if length (trim entry) < (matchLength env)
        then do  -- prompt until valid entry
            liftIO $ putStrLn $ "Word must be at least " ++ (show $ matchLength env) ++ " letters."
            prompt  
        else
            if prev s == ""  
            then do  -- first turn
                liftIO $ putStrLn $ "Okay. Now match " ++ (show $ matchLength env) ++ " letters."
                put $ GameState {prev=entry, gameOver=(gameOver s)}
                return entry
            else
                if isMatch (matchLength env) (prev s) entry
                then do  -- match 
                    put $ GameState {prev=entry, gameOver=(gameOver s)}
                    liftIO $ showMatch (matchLength env) (prev s) entry
                    return entry
                else do  -- mismatch
                    put $ GameState {prev=entry, gameOver=True}
                    return entry

showGameOver :: RWST Env [String] GameState IO ()
showGameOver = liftIO $ putStrLn "---- No Match. Game Over. ----\n"

isMatch :: Int -> String -> String -> Bool
isMatch n prev entry = (take n entry) == (reverse $ take n $ reverse prev)

showMatch :: Int -> String -> String -> IO ()
showMatch len prev new = putStrLn $
    "    MATCH: "
    ++ prev1 ++ "(" ++ prev2 ++ ")" 
    ++ " --> " 
    ++ "(" ++ new1 ++ ")" ++ new2
    where
        prev1  = take (length prev - len) prev
        prev2  = drop (length prev - len) prev
        new1 = take len new
        new2 = drop len new

showHistory :: Int -> [String] -> IO ()
showHistory n ss = putStrLn $ "History (" ++ (show n) ++ "-letter matches):\n    " ++ (concat $ intersperse ", " ss)

data GameState = GameState {prev::String, gameOver::Bool}

data Env = Env {matchLength::Int}

