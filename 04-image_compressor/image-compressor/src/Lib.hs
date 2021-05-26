module Lib
    ( printErrAndReturn
    , someFunc
    ) where

import System.Exit

printErrAndReturn :: String -> Int -> IO ()
printErrAndReturn err i = do
    putStrLn err
    exitWith (ExitFailure i)

someFunc :: IO ()
someFunc = putStrLn "someFunc"