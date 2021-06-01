--
-- EPITECH PROJECT, 2021
-- Image Compressor
-- File description:
-- Main
--

module Main where

import Lib
-- import Pixel
-- import Position 
-- import Color

import System.Environment
import Data.List
import Data.Tuple
import Data.Char
import System.Exit
import System.IO
import Control.Monad

data Position = Position Int Int

instance Show Position where
    show (Position x y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance Eq Position where
    (Position x1 y1) == (Position x2 y2) = x1 == x2 && y1 == y2

instance Num Position where
    (Position x1 y1) + (Position x2 y2) = Position (x1 + x2) (y1 + y2)
    (Position x1 y1) - (Position x2 y2) = Position (x1 - x2) (y1 - y2)
    (Position x1 y1) * (Position x2 y2) = Position (x1 * x2) (y1 * y2)

instance Read Position where
    readsPrec _ input = [(Position (read (takeX input)) (read (takeY input)), takeRest input)]
        where
            takeX :: String -> String
            takeX = takeWhile (/= ',') . tail . dropWhile (/='(')
            takeY :: String -> String
            takeY = takeWhile (/= ')') . tail . dropWhile (/=',')
            takeRest :: String -> String
            takeRest = tail . dropWhile (/= ')')

data Color = Color Double Double Double

instance Show Color where
    show (Color r g b) = "(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

instance Eq Color where
    (Color r1 g1 b1) == (Color r2 g2 b2) = r1 == r2 && g1 == g2 && b1 == b2

instance Num Color where
    (Color r1 g1 b1) + (Color r2 g2 b2) = Color (r1 + r2 / 2) (g1 + g2 / 2) (b1 + b2 / 2)
    (Color r1 g1 b1) - (Color r2 g2 b2) = Color (r1 - r2 / 2) (g1 - g2 / 2) (b1 - b2 / 2)

instance Read Color where
    readsPrec _ input = [(Color (read (takeR input)) (read (takeG input)) (read (takeB input)), takeRest input)]
        where
            takeR :: String -> String
            takeR = takeWhile  (/=',') . tail . dropWhile (/='(')
            takeG :: String -> String
            takeG = takeWhile (/= ',') . tail . dropWhile (/=',')
            takeB :: String -> String
            takeB = takeWhile (/= ')') . tail . dropWhile (/=',')
            takeRest :: String -> String
            takeRest = tail . dropWhile (/= ')')

data Pixel = Pixel Position Color

instance Show Pixel where
    show (Pixel pos col) = show pos ++ " " ++ show col

instance Eq Pixel where
    (Pixel pos1 col1) == (Pixel pos2 col2) = pos1 == pos2 && col1 == col2

instance Num Pixel where
    (Pixel pos1 col1) + (Pixel pos2 col2) = Pixel (pos1 + pos2) (col1 + col2)
    (Pixel pos1 col1) - (Pixel pos2 col2) = Pixel (pos1 - pos2) (col1 - col2)

instance Read Pixel where
    readsPrec _ input = [(Pixel pos col, rest2)]
        where
            [(pos, rest1)] = readsPrec 0 input :: [(Position, String)]
            [(col, rest2)] = readsPrec 0 rest1 :: [(Color, String)]

data Cluster = Cluster Color [Pixel]

instance Show Cluster where
    show (Cluster col pixels) = "--\n" ++ show col ++ "\n-" ++ show pixels

printUsage :: IO()
printUsage = putStrLn "USAGE: ./imageCompressor n e IN\n\n\t\
\n\tnumber of colors in the final image\n\t\
\e\tconvergence limit\n\t\
\IN\tpath to the file containing the colors of the pixels"

euclidianDistance :: (Double, Double, Double) -> (Double, Double, Double) -> Double
euclidianDistance (r1, g1, b1) (r2, g2, b2) = sqrt ((r1 - r2)^2 + (g1 - g2)^2 + (b1 - b2)^2)

addPosition :: Position -> Position -> Position
addPosition p1 p2 = p1 + p2

imageCompressor :: a -> b -> FilePath -> IO ()
imageCompressor n e infile = do
    text <- readFile infile
    putStrLn text

main :: IO ()
main = do
    args <- getArgs
    if length args /= 3 then printUsage >> printErrAndReturn "wrong number of arguments" 84 else (imageCompressor (args!!0) (args!!1) (args!!2))