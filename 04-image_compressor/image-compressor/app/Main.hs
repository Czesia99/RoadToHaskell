{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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
import Text.Read
import Data.List
import Data.Tuple
import Data.Char
import Data.Maybe
import System.Exit
import System.IO
import Control.Monad
import Text.Printf
import System.Random

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
    show (Color r g b) = "(" ++ printf "%.f" r ++ "," ++ printf "%.f" g ++ "," ++ printf "%.f" b ++ ")"

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
            takeB = tail . takeWhile (/=')') . tail . dropWhile (/=',') . tail . dropWhile (/=',')
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
            [(pos, rest1)] = reads input :: [(Position, String)]
            [(col, rest2)] = reads rest1 :: [(Color, String)]

data Cluster = Cluster Color [Pixel]

instance Show Cluster where
    show (Cluster col pixels) = "--\n" ++ show col ++ "\n-\n" ++ intercalate "\n" (map show pixels) --showNoBrackets pixels

--- utils ---

showNoBrackets :: Show a => a -> [Char]
showNoBrackets x = [c | c <- show x, c /= '[' && c/= ']']

getColorR :: Color -> Double
getColorR (Color r g b) = r

getColorG :: Color -> Double
getColorG (Color r g b) = g

getColorB :: Color -> Double
getColorB (Color r g b) = b

getColor :: Pixel -> Color
getColor (Pixel pos col) = col

setColor :: Pixel -> Color -> Pixel
setColor (Pixel pos col) col' = Pixel pos col'

getPosition :: Pixel -> Position
getPosition (Pixel pos col) = pos

setPosition :: Pixel -> Position -> Pixel
setPosition (Pixel pos col) pos' = Pixel pos' col

getClusterColor :: Cluster -> Color
getClusterColor (Cluster col pixels) = col

setClusterColor :: Cluster -> Color -> Cluster
setClusterColor (Cluster col pixels) col' = Cluster col' pixels

getClusterPixels :: Cluster -> [Pixel]
getClusterPixels (Cluster col pixels) = pixels

setClusterPixels :: Cluster -> [Pixel] -> Cluster
setClusterPixels (Cluster col pixels) pixels' = Cluster col pixels'

inputToPixel :: String -> Pixel
inputToPixel input = read input :: Pixel

inputToPixels :: [String] -> [Pixel]
inputToPixels = map inputToPixel

printUsage :: IO()
printUsage = putStrLn "USAGE: ./imageCompressor n e IN\n\n\t\
\n\tnumber of colors in the final image\n\t\
\e\tconvergence limit\n\t\
\IN\tpath to the file containing the colors of the pixels"

eDistColor :: Color -> Color -> Double
eDistColor (Color r1 g1 b1) (Color r2 g2 b2) = sqrt ((r1 - r2)^2 + (g1 - g2)^2 + (b1 - b2)^2)

addPosition :: Position -> Position -> Position
addPosition p1 p2 = p1 + p2

type SelectedColors = [Color]

defineKCentroids :: Int -> [Pixel] -> [Cluster]
defineKCentroids k pixels = createCluster selectedColors []
    where
        createCluster [] c = c
        createCluster (x:xs) c = createCluster xs (Cluster x (regroupPixelsToColor x selectedColors pixels) : c)
        selectedColors = (selectColors k pixels)

regroupPixelsToColor :: Color -> SelectedColors -> [Pixel] -> [Pixel]
regroupPixelsToColor col sc pixels = filter (isNearestPixelToColor col sc) pixels

isNearestPixelToColor :: Color -> SelectedColors -> Pixel -> Bool
isNearestPixelToColor col [] pixel = True
isNearestPixelToColor col (x:xs) pixel
    | eDistColor col (getColor pixel) <= eDistColor x (getColor pixel) = isNearestPixelToColor col xs pixel
    | otherwise = False

selectColors :: Int -> [Pixel] -> [Color]
selectColors n pixels = select n (map getColor pixels) []
    where
        select n (x:xs) selectedColors
            | n == 0 = selectedColors
            | null selectedColors = select (n - 1) xs (x : selectedColors)
            | isColorDifferent x selectedColors = select (n - 1) xs (x : selectedColors)
            | otherwise = select n xs selectedColors

isColorDifferent :: Color -> [Color] -> Bool
isColorDifferent _ [] = True
isColorDifferent c (x:xs)
    | c == x = False
    | otherwise = isColorDifferent c xs

clusterAverage :: [Pixel] -> Color
clusterAverage pixels = Color averageR averageG averageB
    where
        averageR = foldl (+) 0 (map getColorR (map getColor pixels)) / fromIntegral (length pixels)
        averageG = foldl (+) 0 (map getColorG (map getColor pixels)) / fromIntegral (length pixels)
        averageB = foldl (+) 0 (map getColorB (map getColor pixels)) / fromIntegral (length pixels)

replaceCentroid :: [Cluster] -> [Cluster]
replaceCentroid (x:xs) = undefined

isKmeanDone :: Double -> [Cluster] -> [Cluster] -> Bool
isKmeanDone _ [] [] = True
isKmeanDone e (x:xs) (y:ys)
    | eDistColor (getClusterColor x) (getClusterColor y) <= e = isKmeanDone e xs ys
    | otherwise = False

imageCompressor :: Int -> b -> FilePath -> IO ()
imageCompressor k e infile = do
    text <- fmap lines (readFile infile)
    mapM_ print (defineKCentroids k (inputToPixels text))

main :: IO ()
main = do
    args <- getArgs
    if length args /= 3 then printUsage >> printErrAndReturn "wrong number of arguments" 84 else imageCompressor (read (head args) :: Int) (args!!1) (args!!2)