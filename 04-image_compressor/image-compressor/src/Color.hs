module Color where

data Color = Color Double Double Double

instance Show Color where
    show (Color r g b) = "(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

instance Eq Color where
    (Color r1 g1 b1) == (Color r2 g2 b2) = r1 == r2 && g1 == g2 && b1 == b2

instance Num Color where
    (Color r1 g1 b1) + (Color r2 g2 b2) = Color (r1 + r2 / 2) (g1 + g2 / 2) (b1 + b2 / 2)
    (Color r1 g1 b1) - (Color r2 g2 b2) = Color (r1 - r2 / 2) (g1 - g2 / 2) (b1 - b2 / 2)

-- instance Read Color where
--     readsPrec _ input = [(pos)]