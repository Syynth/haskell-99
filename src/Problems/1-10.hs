
{-
Problem 1.
Find the last element of a list
-}
myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs

myLast' = head . reverse

{-
Problem 2.
Find the last but one element of a list
-}
myButLast :: [a] -> a
myButLast = head . tail . reverse

{-
Problem 3.
Find the K'th element of a list
-}
elementAt :: [a] -> Int -> a
elementAt l i = l !! (i - 1)

{-
Problem 4.
Find the number of elements of a list
-}
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = myLength xs + 1

{-
Problem 5.
Reverse a list
-}
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' = foldl (flip (:)) []