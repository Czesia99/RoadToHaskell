module Kmean
    ( Cluster
    , kmeanGraphics
    , kmean
    ) where

import Data.List
import Codec.Picture
import Piksel

data Cluster = Cluster Color [Piksel]

instance Show Cluster where
    show (Cluster col pixels) = "--\n" ++ show col ++ "\n-\n" ++ intercalate "\n" (map show pixels)

type SelectedColors = [Color]

getClusterColor :: Cluster -> Color
getClusterColor (Cluster col pixels) = col

setClusterColor :: Cluster -> Color -> Cluster
setClusterColor (Cluster col pixels) col' = Cluster col' pixels

getClusterPixels :: Cluster -> [Piksel]
getClusterPixels (Cluster col pixels) = pixels

setClusterPixels :: Cluster -> [Piksel] -> Cluster
setClusterPixels (Cluster col pixels) pixels' = Cluster col pixels'

showClusters :: [Cluster] -> IO ()
showClusters [] = return ()
showClusters (x:xs) = do
    print x
    showClusters xs

isColorDifferent :: Color -> [Color] -> Bool
isColorDifferent _ [] = True
isColorDifferent c (x:xs)
    | c == x = False
    | otherwise = isColorDifferent c xs

selectColors :: Int -> [Piksel] -> [Color]
selectColors n pixels = select n (map getColor pixels) []
    where
        select n (x:xs) selectedColors
            | n == 0 = selectedColors
            | null selectedColors = select (n - 1) xs (x : selectedColors)
            | isColorDifferent x selectedColors = select (n - 1) xs (x : selectedColors)
            | otherwise = select n xs selectedColors

isNearestPixelToColor :: Color -> SelectedColors -> Piksel -> Bool
isNearestPixelToColor col [] pixel = True
isNearestPixelToColor col (x:xs) pixel
    | eDistColor col (getColor pixel) <= eDistColor x (getColor pixel) = isNearestPixelToColor col xs pixel
    | otherwise = False

regroupPixelsToColor :: Color -> SelectedColors -> [Piksel] -> [Piksel]
regroupPixelsToColor col sc pixels = filter (isNearestPixelToColor col sc) pixels

isKmeanDone :: Double -> [Cluster] -> [Cluster] -> Bool
isKmeanDone _ [] [] = True
isKmeanDone e (x:xs) (y:ys)
    | eDistColor (getClusterColor x) (getClusterColor y) > e = False
    | otherwise = isKmeanDone e xs ys

replaceCentroids :: [Piksel] -> [Cluster] -> [Cluster]
replaceCentroids pixels clusters  = newClusters clusters []
    where
        newClusters [] nc = reverse nc
        newClusters (x:xs) nc = newClusters xs (Cluster (newColor x) (newPixelGroup x) : nc)
        newColor cluster = colorAverage (getClusterPixels cluster)
        newPixelGroup cluster = regroupPixelsToColor (newColor cluster) (map (colorAverage . getClusterPixels) clusters) pixels

defineKCentroids :: Int -> [Piksel] -> [Cluster]
defineKCentroids k pixels = createCluster selectedColors []
    where
        createCluster [] c = c
        createCluster (x:xs) c = createCluster xs (Cluster x (regroupPixels x) : c)
        selectedColors = selectColors k pixels
        regroupPixels x = regroupPixelsToColor x selectedColors pixels

kmeanGraphics :: Int -> Double -> FilePath -> IO ()
kmeanGraphics k e infile = do
    img <- readImage infile
    case img of
        Left _ -> putStrLn $ "not a supported codec for " ++ infile
        Right dynimg -> do
            putStrLn "image loaded"

kmean :: Int -> Double -> FilePath -> IO ()
kmean k e infile = do
    text <- fmap lines (readFile infile)
    let pixels = inputToPixels text
    doKmean pixels (defineKCentroids k pixels) (replaceCentroids pixels (defineKCentroids k pixels))
        where
            doKmean pixels cx cy
                | isKmeanDone e cx cy = showClusters cy
                | otherwise = doKmean pixels cy (replaceCentroids pixels cy)

-- isPixelUnused :: Piksel -> [Piksel] -> Bool
-- isPixelUnused pixel [] = True
-- isPixelUnused pixel (x:xs)
--     | pixel == x = False
--     | otherwise = isPixelUsed pixel xs

-- regroupPixelsToColor :: Color -> SelectedColors -> [Piksel] -> [Piksel] -> [Piksel]
-- regroupPixelsToColor col sc pixels usedPixels = filter isPixelUnused regroupPixels
--     where regroupPixels = filter (isNearestPixelToColor col sc) pixels