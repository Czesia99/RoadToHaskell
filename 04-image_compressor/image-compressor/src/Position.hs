module Position where

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
