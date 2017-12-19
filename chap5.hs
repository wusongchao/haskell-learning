module Yu
(
    applyTwice',
    zipWith',
    tupleMull',
    map',
    filter',
    foldl',
    foldr',
    mapByFoldl',
    mapByFoldr',
    elem',
    maximum',
    reverse',
    product',
    filterByFold',
    last',
    and',
    removeDeplicate',
    scanl',
    oddSquareSum'
) 
 where

applyTwice' :: (a -> a) -> a -> a
applyTwice' f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

tupleMull' :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
tupleMull' (a1, b1) (a2, b2) = (a1 * a2, b1 * b2)

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x == True = x : filter' f xs
    | otherwise = filter' f xs

-- foldl : f acc x
-- foldr : f x acc

foldl' f acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

foldr' f acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

mapByFoldl' :: (a -> b) -> [a] -> [b]
mapByFoldl' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

mapByFoldr' :: (a -> b) -> [a] -> [b]
mapByFoldr' f xs = foldr (\x acc -> f x : acc) [] xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys 

maximum' :: (Ord a) => [a] -> a
maximum' ls = foldl1 max ls

reverse' :: [a] -> [a]
reverse' ls = foldl (\acc x -> x : acc) [] ls

product' :: (Num a) => [a] -> a
product' xs = foldl (*) 1 xs 

-- ++ is much slower than :
filterByFold' :: (a -> Bool) -> [a] -> [a]
filterByFold' f xs = foldr (\x acc -> if f x == True then x : acc else acc) [] xs

last' :: [a] -> a
last' xs = foldl1 (\_ x -> x) xs

and' :: [Bool] -> Bool
and' xs = foldl (&&) True xs

-- removeDeplicate' :: [a] -> [a]
removeDeplicate' xs = foldr func [] xs
    where func x [] = x : [] 
          func x (a:acc) = if a == x then x : acc else x : a : acc

scanl' f acc [] = [acc]
scanl' f acc (x:xs) = f acc x : scanl' f acc xs


oddSquareSum' :: Integer
oddSquareSum' = sum (takeWhile (<10000) filter odd $ map $ (^2) [1..])
-- imple foldl by foldr
