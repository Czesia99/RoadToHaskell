--
-- EPITECH PROJECT, 2021
-- 01-day1
-- File description:
-- basic haskell functions
--

module My where

mySucc :: Int -> Int
mySucc a = a + 1
-- function which takes one Int as argument and returns its successor.

myIsNeg :: Int -> Bool
myIsNeg a
    | a < 0 = True
    | a >= 0 = False
-- function which takes one Int as argument and returns True if it’s negative or False otherwise.

myAbs :: Int -> Int
myAbs a
    | a < 0 = a * (-1)
    | a >= 0 = a
-- function which takes one Int as argument and returns it’s absolute value.

myMin :: Int -> Int -> Int
myMin a b
    | a - b <= 0  = a
    | b - a <= 0 = b
-- function which takes two Ints as arguments and returns the minimum of the two.

myMax :: Int -> Int -> Int
myMax a b
    | a - b <= 0  = b
    | b - a <= 0 = a
-- I’m sure you can guess what this function returns :)

myTuple :: a -> b -> (a, b)
myTuple a b = (a, b)
-- function which takes two arguments and a return a tuple of those.

myTruple :: a -> b -> c -> (a, b, c)
myTruple a b c = (a, b, c)
-- function which takes three arguments and a return a tuple of those.

myFst :: (a, b) -> a
myFst (a, b) = a
-- function which takes a tuple as argument and returns its first value.

mySnd :: (a, b) -> b
mySnd (a, b) = b
-- function which takes a tuple as argument and returns its second value.

mySwap :: (a, b) -> (b, a)
mySwap (a, b) = (b, a)
-- function which takes a tuple as argument and returns a new tuple, with it’s two values swaped.

myHead :: [a] -> a
myHead [] = error "empty list"
myHead (x:xs) = x
-- function which takes a list as argument and returns its first value. 
-- If the list is empty, an exception is raised with the function error.

myTail :: [a] -> a
myTail [] = error "empty list"
myTail [x] = x
myTail (_:xs) = myTail xs
-- function which takes a list as argument and returns a new list without
-- its first element. If the list is empty, an exception is raised with the function error.

myLength :: [a] -> Int
myLength [] = 0
myLength [x] = 1
myLength (x:xs) = 1 + myLength xs
-- function which takes a list as argument and returns the number of
-- elements in the list.

myNth :: [a] -> Int -> a
myNth [] n = error "empty list" 
myNth (x:xs) n
    | myIsNeg n = error "index cannot be a negative number" 
    | myLength (x:xs) < n = error "index out of range"
    | n == 0 = x
    | otherwise = myNth xs (n-1)
-- function which takes a list and an Int (N) as argument and returns the element at index N in the list,
-- or an error if the index is too large or negative.

myTake :: Int -> [a] -> [a]
myTake n [] = error "empty list"
myTake n (x:xs)
    | myIsNeg n = error "index cannot be a negative number"
    | myLength (x:xs) < n = (x:xs) 
    | n == 1 = [x]
    | otherwise = x : myTake (n - 1) xs
-- A function which takes an Int (N) and a list and returns a list with the Nth first elements of the list.
-- If the list is too short the whole list is returned.

myDrop :: Int -> [a] -> [a]
myDrop n [] = error "empty list"
myDrop n (x:xs)
    | myIsNeg n = error "index cannot be a negative number"
    | myLength (x:xs) < n = []
    | n == 1 = xs
    | otherwise = myDrop (n - 1) xs
-- function which takes an Int (n) and a list and returns a list without the N first elements.
-- If the list is tooshort an empty list is returned.

myAppend :: [a] -> [a] -> [a]
myAppend [] x = x
myAppend (x:xs) y = x : myAppend xs y
-- function which takes two lists and returns a new list with the second list appened to the first one.

myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myTail (x:xs) : (myReverse (myTake (myLength (x:xs) - 1) (x:xs)))
-- function which takes a list and returns a list with all its elements in reverse order.

myInit :: [a] -> [a]
myInit [] = error "empty list"
myInit [x] = []
myInit (x:xs) = (myTake (myLength (x:xs) - 1) (x:xs))
-- function which takes a list and returns a list with all its elements except the last one,
-- or an error if the list is empty.

myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (x:xs) = myNth (x:xs) (myLength (x:xs) - 1)
-- function which takes a list and returns its last element, or an error if the list is empty.

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys
-- function which takes two lists as arguments, and returns a list of tuples.
-- The list produced is as long as the shortest list.

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([],[])
myUnzip ((a,b):xs) = (a:(myFst rest), b:(mySnd rest))
    where rest = myUnzip xs
-- function which takes a list of tuples, and return a tuple of lists.

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs
-- function which takes a function and a list, and apply this function to every element of the list.

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs) 
    | f x = x : myFilter f xs
    | otherwise = myFilter f xs
-- function which takes a predicate (a function returning a boolean value) and a list,
-- and returns a list of all the elements for which the predicate has returned True.

myFilterFalse :: (a -> Bool) -> [a] -> [a]
myFilterFalse _ [] = []
myFilterFalse f (x:xs) 
    | f x = myFilterFalse f xs
    | otherwise =  x : myFilterFalse f xs
-- function which takes a predicate (a function returning a boolean value) and a list,
-- and returns a list of all the elements for which the predicate has returned False.

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f a [] = a
myFoldl f a (x:xs) = myFoldl f (f a x) xs
-- function which takes a function, a starting value and a list as argument.
-- It takes the second argument and the first item of the list and applies the function to them,
-- then feeds the function with this result and the second argument and so on

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f a [] = a
myFoldr f a (x:xs) = f x (myFoldr f a xs)
-- Like myFoldl, but from right to left.

mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan _ [] = ([],[])
mySpan f x = (myFilter f x, myFilterFalse f x)
-- function which takes a predicate and a list as argument, and returns a tuple of lists, with in the first list the
-- elements for which the predicate returns true, and in the second list the other elements.

myQuickSort :: (a -> a -> Bool) -> [a] -> [a]
myQuickSort _ [] = []
myQuickSort f (x:xs) = (myAppend (myAppend false [x]) true)
    where 
        true = myQuickSort f (myFilter (f x) xs)
        false = myQuickSort f (myFilterFalse (f x) xs)
-- A function which takes a predicate and a list as arguments,
-- and returns the list sorted according to the predicate.