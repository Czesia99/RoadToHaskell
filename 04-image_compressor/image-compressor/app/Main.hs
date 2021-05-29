--
-- EPITECH PROJECT, 2021
-- Image Compressor
-- File description:
-- Main
--

module Main where

import Lib
import Position 
import Color

import System.Environment
import Data.List
import Data.Tuple
import Data.Char
import System.Exit
import System.IO
import Control.Monad

printUsage :: IO()
printUsage = putStrLn "USAGE: ./imageCompressor n e IN\n\n\t\
\n\tnumber of colors in the final image\n\t\
\e\tconvergence limit\n\t\
\IN\tpath to the file containing the colors of the pixels"

euclidianDistance :: (Double, Double, Double) -> (Double, Double, Double) -> Double
euclidianDistance (r1,g1,b1) (r2,g2,b2) = sqrt ((r1 - r2)^2 + (g1 - g2)^2 + (b1 - b2)^2)

imageCompressor :: a -> b -> FilePath -> IO ()
imageCompressor n e infile = do
    text <- readFile infile
    putStrLn text

main :: IO ()
main = do
    args <- getArgs
    if length args /= 3 then printUsage >> printErrAndReturn "wrong number of arguments" 84 else (imageCompressor (args!!0) (args!!1) (args!!2))