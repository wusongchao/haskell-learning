module Yu
(
    isIn',
    encode',
    digitSum',
    firstTo',
    findKey'
) 
where

import Data.List
import Data.Char

isIn' :: (Eq a) => [a] -> [a] -> Bool
isIn' needle haystack = any (needle `isPrefixOf`) (tails haystack)

encode' :: Int -> String -> String
encode' offset message = map (chr.(+offset).ord) message

digitSum' :: Int -> Int
digitSum' num = (sum.map digitToInt.show) num

firstTo' :: Int -> Maybe Int
firstTo' n = find (\x -> digitSum' x == n) [1..]

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v 
findKey' key [] = Nothing
findKey' key xs = foldl (\acc (k, v) -> if key == k then Just v else acc) Nothing xs