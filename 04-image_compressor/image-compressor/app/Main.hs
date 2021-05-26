module Main where

import Lib
import System.Environment
import System.Exit

printUsage :: IO()
printUsage = putStrLn "USAGE: ./imageCompressor n e IN\n\n\t\
\n\tnumber of colors in the final image\n\t\
\e\tconvergence limit\n\t\
\IN\tpath to the file containing the colors of the pixels"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> printUsage >> printErrAndReturn "Wrong number of arguments" 84