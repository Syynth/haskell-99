import Data.List
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

{-
Problem 6.
Determine whether a list is a palidrome
-}
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = x == last xs && (isPalindrome . init) xs

isPalindrome' l = l == (reverse l)

{-
Problem 7.
Flatten nested lists
-}
data NL a = El a | L [NL a]

flatten :: NL a -> [a]
flatten (El a) = [a]
flatten (L ls) = foldl (\ls' nl -> ls' ++ flatten nl) [] ls

{-
Problem 8.
Eliminate consecutive duplicates
-}
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:zs) = if x == y then (compress $ y : zs) else x : (compress $ y : zs)

compress' :: (Eq a) => [a] -> [a]
compress' = map head . group

{-
Problem 9.
Pack consecutive duplicates of list elements into sublists
-}
--pack :: (Eq a) => [a] -> [[a]]
--pack = group
