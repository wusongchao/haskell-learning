module Yu
(
    addVectors,
    length',
    reload,
    max',
    find',
    describeList
) where

addVectors :: (Num t) => (t, t) -> (t, t) -> (t, t)
addVectors (x1, y1) (x2, y2) = (x1 + y1, x2 + y2)

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

reload :: [Char] -> [Char]
reload "" = "empty"
reload all@(x:xs) = xs ++ [x]

max' :: (Ord a) => a -> a -> a
max' a bj
    | otherwise = a

find' :: (Eq a, Num a) => [a] -> Bool
find' [] = False
find' (x:xs)
    | x == target = True
    | otherwise = find' xs
    where target = 100

describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of [] -> "empty"
                                              [x] -> "one"
                                              xs -> "long"                                  