--
-- EPITECH PROJECT, 2021
-- wolfram
-- File description:
-- Cellular Automaton - Wolfram
--

import Text.Read
import System.Environment
import System.Exit

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

checkArgs :: [(String, Int)] -> [Int]
checkArgs args = [getRule args, getStart args, getLines args, getWindow args, getMove args]

wolfram :: [Int] -> IO ()
wolfram [] = return ()
wolfram [a] = return ()
wolfram [rule, start, lines, window, move] = do
    print rule
    print start
    print lines
    print window
    print move

-- readInt :: [Char] -> Maybe Int
-- readInt = readMaybe

getRule :: [(String, Int)] -> Int
getRule [] = 0
getRule ((x, y): xs)
    | x == "--rule" = y
    | otherwise = getRule xs

getStart :: [(String, Int)] -> Int
getStart [] = 0
getStart ((x, y): xs)
    | x == "--start" = y
    | otherwise = getStart xs

getLines :: [(String, Int)] -> Int
getLines [] = 0
getLines ((x, y): xs)
    | x == "--lines" = y
    | otherwise = getLines xs

getWindow :: [(String, Int)] -> Int
getWindow [] = 80
getWindow ((x, y): xs)
    | x == "--window" = y
    | otherwise = getWindow xs

getMove :: [(String, Int)] -> Int
getMove [] = 0
getMove ((x, y): xs)
    | x == "--move" = y
    | otherwise = getMove xs

makeTupleArgs :: [String] -> [(String, Int)]
makeTupleArgs [] = []
makeTupleArgs (x:y:xs) = (x, read y :: Int) : makeTupleArgs xs


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> printUsage >> printErrAndReturn "Invalid number of arguments" 84
        a -> wolfram (checkArgs (makeTupleArgs args))