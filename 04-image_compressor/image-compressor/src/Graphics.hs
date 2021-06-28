module Graphics 
    ( kmeanGraphics
    ) where

import Codec.Picture

kmeanGraphics :: Int -> Double -> FilePath -> IO ()
kmeanGraphics k e infile = do
    img <- readImage infile
    case img of
        Left _ -> putStrLn $ "not a supported codec for " ++ infile
        Right dynimg -> do
            putStrLn "image loaded"