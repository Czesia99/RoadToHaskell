--
-- EPITECH PROJECT, 2021
-- wolfram
-- File description:
-- Cellular Automaton - Wolfram
--

import Text.Read
import Data.Maybe
import System.Environment
import System.Exit


type Cell = Bool
type CellLine = [Cell]
type Rule = Cell -> Cell -> Cell -> Cell

------ print functions ------

printErrAndReturn :: String -> Int -> IO ()
printErrAndReturn err i = do
    putStrLn err
    exitWith (ExitFailure i)

printUsage :: IO ()
printUsage = putStrLn "Usage:\n wolfram --rule n [--options]\n options:\n\t\
\--rule: the ruleset to use (Mandatory)\n\t\
\--start: the generation number at which to start the display (default value is 0)\n\t\
\--lines: the number of lines to display. When homited, the program never stops.\n\t\
\--window: the number of cells to display on each line (line width). If even,the central cell is displayed in the next cell on the right. The default value is 80.\n\t\
\--move: a translation to apply on the window. If negative, the window is translated to the left. If positive, it’s translated to the right."

------ arguments management functions ------

readInt :: String -> Maybe Int
readInt = readMaybe

makeTupleArgs :: [String] -> [(String, Int)]
makeTupleArgs [] = []
makeTupleArgs (x:y:xs) = (x, read y :: Int) : makeTupleArgs xs

checkFlagValidity :: String -> Bool
checkFlagValidity f
    | f == "--rule" = True
    | f == "--start" = True
    | f == "--lines" = True
    | f == "--window" = True
    | f == "--move" = True
    | otherwise = False

checkFlagValue :: String -> Bool
checkFlagValue v
    | isNothing (readInt v) = False
    | otherwise = True

checkOptions :: [String] -> Bool
checkOptions [] = True
checkOptions [x] = checkFlagValue x
checkOptions (x:y:xs)
    | checkFlagValidity x && checkFlagValue y = checkOptions xs
    | otherwise = False

defaultFlagValue :: String -> Int
defaultFlagValue f
    | f == "--start" = 0
    | f == "--lines" = -1
    | f == "--window" = 80
    | f == "--move" = 0

getFlagValue :: [(String, Int)] -> String -> Int
getFlagValue [] f = defaultFlagValue f
getFlagValue ((x, y): xs) flag
    | x == flag = y
    | otherwise = getFlagValue xs flag

getOptions :: [(String, Int)] -> [Int]
getOptions args = [
    getFlagValue args "--rule",
    getFlagValue args "--start",
    getFlagValue args "--lines",
    getFlagValue args "--window",
    getFlagValue args "--move"
    ]

------ real stuff ------
-- toBin :: Int -> [Int]
-- toBin 0 = [0]
-- toBin 1 = [1]
-- toBin n
--     | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]
--     | otherwise = toBin (n `div` 2) ++ [1]

toBinary :: Int -> [Int]
toBinary i = toBin 8 [] i where
    toBin 0 binary _ = binary
    toBin n binary i = toBin (n - 1) (rest:binary) quotient where
        quotient = fst (i `divMod` 2)
        rest = snd (i `divMod` 2)

wolfram :: [Int] -> IO ()
wolfram [] = return ()
wolfram [a] = return ()
wolfram [rule, start, lines, window, move] = do
    putStrLn "------ in wolfram ------"
    print rule
    print start
    print lines
    print window
    print move
    putStrLn "--- rule converted in bin ------"
    mapM_ print (toBinary rule)

main :: IO ()
main = do
    args <- getArgs
    mapM_ putStrLn args
    case args of
        [] -> printUsage >> invalidArguments
        a -> if checkOptions args then doWolfram args  else printUsage >> invalidArguments
        where
            doWolfram args = wolfram (getOptions (makeTupleArgs args))
            invalidArguments = printErrAndReturn "Invalid arguments" 84