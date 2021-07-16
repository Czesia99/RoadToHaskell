module Piksel
    ( Position
    , Color
    , Piksel
    , getColorR
    , getColorG
    , getColorB
    , getColor
    , setColor
    , getPosition
    , setPosition
    , addPosition
    , distColor
    , eDistColor
    , groupUnusedPixels
    , colorAverage
    , inputToPixels
    ) where

import Text.Printf

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

type UsedPixels = [Piksel]

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

addPosition :: Position -> Position -> Position
addPosition p1 p2 = p1 + p2

distColor :: Color -> Color -> Double
distColor (Color r1 g1 b1) (Color r2 g2 b2) = (r1 - r2)^2 + (g1 - g2)^2 + (b1 - b2)^2

eDistColor :: Color -> Color -> Double
eDistColor (Color r1 g1 b1) (Color r2 g2 b2) = sqrt ((r1 - r2)^2 + (g1 - g2)^2 + (b1 - b2)^2)

inputToPixel :: String -> Piksel
inputToPixel input = read input :: Piksel

inputToPixels :: [String] -> [Piksel]
inputToPixels = map inputToPixel

isPixelUnused :: UsedPixels -> Piksel -> Bool
isPixelUnused [] pixel = True
isPixelUnused (x:xs) pixel
    | pixel == x = False
    | otherwise = isPixelUnused xs pixel

groupUnusedPixels :: [Piksel] -> UsedPixels -> [Piksel]
groupUnusedPixels pixels usedPixels = filter (isPixelUnused usedPixels) pixels

colorAverage :: [Piksel] -> Color
colorAverage pixels = Color averageR averageG averageB
    where
        averageR = sum (map (getColorR . getColor) pixels) / fromIntegral (length pixels)
        averageG = sum (map (getColorG . getColor) pixels) / fromIntegral (length pixels)
        averageB = sum (map (getColorB . getColor) pixels) / fromIntegral (length pixels)