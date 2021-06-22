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
import Codec.Picture
import Codec.Picture.Types

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

data Piksel = Piksel Position Color

instance Show Piksel where
    show (Piksel pos col) = show pos ++ " " ++ show col

instance Eq Piksel where
    (Piksel pos1 col1) == (Piksel pos2 col2) = pos1 == pos2 && col1 == col2

instance Num Piksel where
    (Piksel pos1 col1) + (Piksel pos2 col2) = Piksel (pos1 + pos2) (col1 + col2)
    (Piksel pos1 col1) - (Piksel pos2 col2) = Piksel (pos1 - pos2) (col1 - col2)

instance Read Piksel where
    readsPrec _ input = [(Piksel pos col, rest2)]
        where
            [(pos, rest1)] = reads input :: [(Position, String)]
            [(col, rest2)] = reads rest1 :: [(Color, String)]

data Cluster = Cluster Color [Piksel]

instance Show Cluster where
    show (Cluster col pixels) = "--\n" ++ show col ++ "\n-\n" ++ intercalate "\n" (map show pixels)

--- utils ---

getColorR :: Color -> Double
getColorR (Color r g b) = r

getColorG :: Color -> Double
getColorG (Color r g b) = g

getColorB :: Color -> Double
getColorB (Color r g b) = b

getColor :: Piksel -> Color
getColor (Piksel pos col) = col

setColor :: Piksel -> Color -> Piksel
setColor (Piksel pos col) col' = Piksel pos col'

getPosition :: Piksel -> Position
getPosition (Piksel pos col) = pos

setPosition :: Piksel -> Position -> Piksel
setPosition (Piksel pos col) pos' = Piksel pos' col

getClusterColor :: Cluster -> Color
getClusterColor (Cluster col pixels) = col

setClusterColor :: Cluster -> Color -> Cluster
setClusterColor (Cluster col pixels) col' = Cluster col' pixels

getClusterPixels :: Cluster -> [Piksel]
getClusterPixels (Cluster col pixels) = pixels

setClusterPixels :: Cluster -> [Piksel] -> Cluster
setClusterPixels (Cluster col pixels) pixels' = Cluster col pixels'

inputToPixel :: String -> Piksel
inputToPixel input = read input :: Piksel

inputToPixels :: [String] -> [Piksel]
inputToPixels = map inputToPixel

printUsage :: IO()
printUsage = putStrLn "USAGE: ./imageCompressor n e IN\n\n\t\
\n\tnumber of colors in the final image\n\t\
\e\tconvergence limit\n\t\
\IN\tpath to the file containing the colors of the pixels"

showClusters :: [Cluster] -> IO ()
showClusters [] = return ()
showClusters (x:xs) = do
    print x
    showClusters xs

eDistColor :: Color -> Color -> Double
eDistColor (Color r1 g1 b1) (Color r2 g2 b2) = sqrt ((r1 - r2)^2 + (g1 - g2)^2 + (b1 - b2)^2)

addPosition :: Position -> Position -> Position
addPosition p1 p2 = p1 + p2

-- isPixelUnused :: Piksel -> [Piksel] -> Bool
-- isPixelUnused pixel [] = True
-- isPixelUnused pixel (x:xs)
--     | pixel == x = False
--     | otherwise = isPixelUsed pixel xs

-- regroupPixelsToColor :: Color -> SelectedColors -> [Piksel] -> [Piksel] -> [Piksel]
-- regroupPixelsToColor col sc pixels usedPixels = filter isPixelUnused regroupPixels
--     where regroupPixels = filter (isNearestPixelToColor col sc) pixels

type SelectedColors = [Color]

defineKCentroids :: Int -> [Piksel] -> [Cluster]
defineKCentroids k pixels = createCluster selectedColors []
    where
        createCluster [] c = c
        createCluster (x:xs) c = createCluster xs (Cluster x (regroupPixels x) : c)
        selectedColors = selectColors k pixels
        regroupPixels x = regroupPixelsToColor x selectedColors pixels

regroupPixelsToColor :: Color -> SelectedColors -> [Piksel] -> [Piksel]
regroupPixelsToColor col sc pixels = filter (isNearestPixelToColor col sc) pixels

isNearestPixelToColor :: Color -> SelectedColors -> Piksel -> Bool
isNearestPixelToColor col [] pixel = True
isNearestPixelToColor col (x:xs) pixel
    | eDistColor col (getColor pixel) <= eDistColor x (getColor pixel) = isNearestPixelToColor col xs pixel
    | otherwise = False

selectColors :: Int -> [Piksel] -> [Color]
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

clusterAverage :: [Piksel] -> Color
clusterAverage pixels = Color averageR averageG averageB
    where
        averageR = sum (map (getColorR . getColor) pixels) / fromIntegral (length pixels)
        averageG = sum (map (getColorG . getColor) pixels) / fromIntegral (length pixels)
        averageB = sum (map (getColorB . getColor) pixels) / fromIntegral (length pixels)

replaceCentroids :: [Piksel] -> [Cluster] -> [Cluster]
replaceCentroids pixels clusters  = newClusters clusters []
    where
        newClusters [] nc = reverse nc
        newClusters (x:xs) nc = newClusters xs (Cluster (newColor x) (newPixelGroup x) : nc)
        newColor cluster = clusterAverage (getClusterPixels cluster)
        newPixelGroup cluster = regroupPixelsToColor (newColor cluster) (map (clusterAverage . getClusterPixels) clusters) pixels

isKmeanDone :: Double -> [Cluster] -> [Cluster] -> Bool
isKmeanDone _ [] [] = True
isKmeanDone e (x:xs) (y:ys)
    | eDistColor (getClusterColor x) (getClusterColor y) > e = False
    | otherwise = isKmeanDone e xs ys

checkOptionsValues :: [String] -> Bool
checkOptionsValues [k,e,_]
    | isNothing (readInt k) = False
    | isNothing (readDouble e) = False
    | otherwise = True

-- checkOptionsValues [k,e,_,g]
--     | isNothing (readInt k) = False
--     | isNothing (readDouble e) = False
--     | g /= "--graphics" && g/= "-g" = False
--     | otherwise = True

checkOptions :: [String] -> Bool
checkOptions args
    | length args == 3 = checkOptionsValues args
    | otherwise = False
    -- | length args == 3 || length args == 4 = checkOptionsValues args

readInt :: String -> Maybe Int
readInt = readMaybe

readDouble :: String -> Maybe Double
readDouble = readMaybe

imageCompressor :: Int -> Double -> FilePath -> IO ()
imageCompressor k e infile = do
    text <- fmap lines (readFile infile)
    let pixels = inputToPixels text
    doKmean pixels (defineKCentroids k pixels) (replaceCentroids pixels (defineKCentroids k pixels))
        where
            doKmean pixels cx cy
                | isKmeanDone e cx cy = showClusters cy
                | otherwise = doKmean pixels cy (replaceCentroids pixels cy)

-- imageCompressorGraphics :: Int -> Double -> FilePath -> IO ()
-- imageCompressorGraphics k e infile = do
--     img <- readImage infile
--     case img of
--         Left _ -> putStrLn $ "not a supported codec for " ++ infile
--         Right dynimg -> do
--             putStrLn "image loaded"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> printUsage
        a -> if checkOptions args then imageCompressor (read (head args) :: Int) (read (args!!1) :: Double) (args!!2) else printUsage >> printErrAndReturn "wrong number of arguments" 84