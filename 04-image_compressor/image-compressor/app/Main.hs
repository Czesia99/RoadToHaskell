--
-- EPITECH PROJECT, 2021
-- Image Compressor
-- File description:
-- Main
--

module Main where

import Lib
import Kmean
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> printUsage
        [k,e,fp] -> if checkOptions args then kmean (read k :: Int) (read e :: Double) fp else err
        [k,e,fp,g] -> if checkOptions args then kmeanGraphics (read k :: Int) (read e :: Double) fp else err
        where 
            err = printUsage >> printErrAndReturn "wrong arguments" 84