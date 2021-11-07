module Lib
    ( removeSpaces
    , printErrAndReturn
    , printUsage
    ) where

import Data.Char
import System.Exit

removeSpaces :: String -> String -> String
removeSpaces [] r = reverse r
removeSpaces (x:xs) r
    | isSpace x = removeSpaces xs r
    | otherwise = removeSpaces xs (x:r)

printErrAndReturn :: String -> Int -> IO ()
printErrAndReturn err i = putStrLn err >> exitWith (ExitFailure i)

printUsage :: IO ()
printUsage = putStrLn "USAGE\n\t./funEvalExpr eval\n\
\DESCRIPTION\n\teval\t\
\string that contains your operations"
