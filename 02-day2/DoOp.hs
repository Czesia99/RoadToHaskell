--
-- EPITECH PROJECT, 2021
-- 02-day2
-- File description:
-- first haskell program
--

import My
import Text.Read
import Control.Monad
import System.Environment
import System.Exit
import Data.Maybe

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = e == x || myElem e xs

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv a b = Just (a `div` b)

safeNth :: [a] -> Int -> Maybe a
safeNth [] _ = Nothing
safeNth (x:xs) n
    | myIsNeg n = Nothing
    | length (x:xs) < n = Nothing
    | n == 0 = Just x
    | otherwise = safeNth xs (n-1)

safeSucc :: Maybe Int -> Maybe Int
safeSucc Nothing = Nothing
safeSucc (Just a) = Just (a + 1)

safeSucc' :: Maybe Int -> Maybe Int
safeSucc' Nothing = Nothing
safeSucc' a = fmap (+1) a
-- safeSucc using fmap

safeSucc'' :: Maybe Int -> Maybe Int
safeSucc'' n = Just n >>= safeSucc
-- safeSucc using >>= operator

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup k ((a,b):xs)
    | k == a = Just b
    | otherwise = myLookup k xs

maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo f _ Nothing = Nothing
maybeDo f Nothing _ = Nothing 
maybeDo f (Just a) (Just b) = Just (f a b)

-- maybeDo' :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
-- maybeDo' f a b = (Just a) >>= maybeDo

-- maybeDo'' :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
-- maybeDo'' f a b =
--     do  a <- Just a
--         b <- Just b
--         return (f a b)

readInt :: [Char] -> Maybe Int
readInt = readMaybe

getLineLength :: IO Int
getLineLength = length <$> getLine

printAndGetLength :: String -> IO Int
printAndGetLength a = do
    putStrLn a
    return (length a)

printBox :: Int -> IO ()
printBox n
    | n <= 0 = return ()
    | otherwise = do
        printBoxEdge
        printBoxLines
        printBoxEdge
        where
            printBoxEdge = putStrLn ("+" ++ replicate (n * 2 - 2) '-' ++ "+")
            printBoxLines = replicateM_ (n-2) $ putStrLn ("|" ++ replicate (n * 2 - 2) ' ' ++ "|")

concatLines :: Int -> IO String
concatLines n
    | n <= 0 = return ""
    | otherwise = fmap concat (replicateM n getLine)


getInt :: IO (Maybe Int)
getInt = readMaybe <$> getLine

doop :: [String] -> Maybe Int
doop [a, op, b]
    | op == "+" = maybeDo (+) (readInt a) (readInt b)
    | op == "-" = maybeDo (-) (readInt a) (readInt b)
    | op == "*" = maybeDo (*) (readInt a) (readInt b)
    | op == "/" = maybeDo div (readInt a) (readInt b)
    | op == "%" = maybeDo mod (readInt a) (readInt b)
    | otherwise = Nothing

main :: IO ()
main = do
    args <- getArgs
    case args of
        [a, op, b] -> print (fromJust (doop args))
        ["-h"] -> usage where usage = putStrLn "usage : ./doop Int (either + - * / or %) Int \n IF YOU USE * DON'T FORGET QUOTES\n"
        [] -> return ()
        [_] -> return ()


-- main :: IO ()
-- main = getArgs >>= parse >>= putStr . doop

-- doop  = maybeDo 

-- parse ["-h"] = usage   >> exit
-- parse []     = getContents
-- parse a b c = print $ a (b) c

-- usage   = putStrLn "Usage: doop Int operator Int"
-- exit    = exitSuccess
-- die     = exitWith (ExitFailure 84)
