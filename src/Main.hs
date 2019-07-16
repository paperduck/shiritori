module Main where

-- reader: number of letters to match at tail and head
-- writer: term history
-- state:  game state

import Control.Monad.RWS.Strict
import Data.Foldable            (traverse_)
import Data.List                (intersperse)
import Data.List.Extra          (trim)
import Text.Read                (readMaybe)

-- Mutable game state
data GameState = GameState {mPrev :: Maybe String, gameOver::Bool}

-- Read-only settings
data Env = Env {matchLength::Int}

-- for judging entries
data EntryType = EmptyEntry | ShortEntry String | FirstEntry String String | Match String | NonMatch

-- Start a new game
main :: IO ()
main = do
    n <- go
    (s,w) <- execRWST play (Env {matchLength=n}) (GameState {mPrev=Nothing, gameOver=False})
    putStrLn $ describeHistory n w
    where
        go = do
            putStrLn "How many letters should match? [default: 1] "
            n <- getLine
            if length (trim n) == 0  -- allow default value
            then putStrLn "Using default" >> return 1
            else case readMaybe n :: Maybe Int of
                Nothing -> putStrLn warning >> go
                Just n -> if n < 1
                    then putStrLn warning >> go
                    else return n
            where warning = "Please enter a non-zero whole number."

-- Game loop
play :: RWST Env [String] GameState IO ()
play = do
    s <- get
    if gameOver s
    then liftIO $ putStrLn gameOverMsg
    else do
        listen prompt >>= (\(a,w) -> tell [a])
        play

-- Get input from user and return it
prompt :: RWST Env [String] GameState IO String
prompt = do
    s <- get
    env <- ask
    liftIO $ putStrLn "\nEnter a word: "
    entry <- liftIO $ getLine
    case judge (matchLength env) (mPrev s) (trim entry) of
        EmptyEntry -> prompt
        ShortEntry msg -> do
            liftIO . putStrLn $ msg
            prompt  
        FirstEntry prev msg -> do
            liftIO $ putStrLn $ msg
            put $ GameState {mPrev = Just prev, gameOver=(gameOver s)}
            return entry
        Match prev -> do
            put $ GameState {mPrev = Just (trim entry), gameOver=(gameOver s)}
            liftIO $ putStrLn $ describeMatch (matchLength env) prev (trim entry)
            return entry
        NonMatch -> do
            put $ GameState {mPrev = (mPrev s), gameOver=True}
            return entry

-- Judge an entry
judge :: Int -> Maybe String -> String -> EntryType
judge matchLength mPrev new
    | length new == 0 = EmptyEntry
    | length new < (matchLength) = ShortEntry $ "Word must be at least " ++ (show matchLength) ++ " letters."
    | otherwise = case mPrev of
        Just prev -> if isMatch matchLength prev new
            then Match prev
            else NonMatch
        Nothing -> FirstEntry new $ "Okay. Now match " ++ (show matchLength) ++ " letters."

-- Describe game over
gameOverMsg :: String
gameOverMsg = "---- No Match. Game Over. ----\n"

-- Judge if new entry is a match or not
isMatch :: Int -> String -> String -> Bool
isMatch n prev entry = (take n entry) == (reverse $ take n $ reverse prev)

-- Describe a successful match as a String message
describeMatch :: Int -> String -> String -> String
describeMatch len prev new = "    MATCH: "
    ++ prev1 ++ "(" ++ prev2 ++ ")" 
    ++ " --> " 
    ++ "(" ++ new1 ++ ")" ++ new2
    where
        prev1  = take (length prev - len) prev
        prev2  = drop (length prev - len) prev
        new1 = take len new
        new2 = drop len new

-- Show word history
describeHistory :: Int -> [String] -> String
describeHistory n ss = "History (" ++ (show n) ++ "-letter matches):\n    " ++ (concat $ intersperse ", " ss)

