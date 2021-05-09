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
checkOptions (x:y:xs)
    | checkFlagValidity x && checkFlagValue y = checkOptions xs
    | otherwise = False

getFlagValue :: [(String, Int)] -> String -> Int
getFlagValue [] _ = 0
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

main :: IO ()
main = do
    args <- getArgs
    mapM_ putStrLn args
    case args of
        [] -> printUsage >> printErrAndReturn "Invalid number of arguments" 84
        a -> if checkOptions args then wolfram (getOptions (makeTupleArgs args)) else printUsage >> printErrAndReturn "Invalid arguments" 84