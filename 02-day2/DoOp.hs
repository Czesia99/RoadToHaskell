--
-- EPITECH PROJECT, 2021
-- 02-day2
-- File description:
-- first haskell program
--

import My

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

-- safeSucc'' :: Maybe Int -> Maybe Int
-- safeSucc'' Nothing = Nothing
-- safeSucc'' >>= (a + 1)