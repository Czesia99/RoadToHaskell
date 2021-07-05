module Kmean
    ( Cluster
    , kmean
    ) where

import Data.List
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
    | col == x = isNearestPixelToColor col xs pixel
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
replaceCentroids pixels clusters  = newClusters clusters [] []
    where
        newClusters [] _ nc = reverse nc
        newClusters (x:xs) usedPixels nc = newClusters xs (newPixelGroup x usedPixels ++ usedPixels) (Cluster (newColor x) (newPixelGroup x usedPixels) : nc)
        newColor cluster = colorAverage (getClusterPixels cluster)
        newPixelGroup cluster usedPixels = regroupPixelsToColor (newColor cluster) (map (colorAverage . getClusterPixels) clusters) (groupUnusedPixels pixels usedPixels)

defineKCentroids :: Int -> [Piksel] -> [Cluster]
defineKCentroids k pixels = createCluster selectedColors [] []
    where
        createCluster [] _ c = c
        createCluster (x:xs) usedPixels c = createCluster xs (regroupPixels x usedPixels ++ usedPixels) (Cluster x (regroupPixels x usedPixels) : c)
        selectedColors = selectColors k pixels
        regroupPixels x usedPixels = regroupPixelsToColor x selectedColors (groupUnusedPixels pixels usedPixels)

kmean :: Int -> Double -> FilePath -> IO ()
kmean k e infile = do
    text <- fmap lines (readFile infile)
    let pixels = inputToPixels text
    doKmean pixels (defineKCentroids k pixels) (replaceCentroids pixels (defineKCentroids k pixels))
        where
            doKmean pixels cx cy
                | isKmeanDone e cx cy = showClusters cy
                | otherwise = doKmean pixels cy (replaceCentroids pixels cy)