module Lib
    ( printErrAndReturn
    , printUsage
    , readInt
    , readDouble
    , checkOptions
    ) where

import Text.Read
import Data.Maybe
import System.Exit

printErrAndReturn :: String -> Int -> IO ()
printErrAndReturn err i = do
    putStrLn err
    exitWith (ExitFailure i)

printUsage :: IO()
printUsage = putStrLn "USAGE: ./imageCompressor n e IN\n\n\t\
\n\tnumber of colors in the final image\n\t\
\e\tconvergence limit\n\t\
\IN\tpath to the file containing the colors of the pixels"

readInt :: String -> Maybe Int
readInt = readMaybe

readDouble :: String -> Maybe Double
readDouble = readMaybe

checkOptionsValues :: [String] -> Bool
checkOptionsValues [k,e,_]
    | isNothing (readInt k) = False
    | isNothing (readDouble e) = False
    | otherwise = True

checkOptionsValues [k,e,_,g]
    | isNothing (readInt k) = False
    | isNothing (readDouble e) = False
    | g /= "--graphics" && g/= "-g" = False
    | otherwise = True

checkOptions :: [String] -> Bool
checkOptions args
    | length args == 3 || length args == 4 = checkOptionsValues args
    | otherwise = False