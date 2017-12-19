module Yu
(
    Point(..),
    Person(..),
    Vector(..),
    move',
    vectorPlus',
    -- LockerState,
    -- Code,
    -- LockerMap,
    lockerLookup
)
where

import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of 
        Nothing -> Left $ "locker" ++ show lockerNumber ++ " doesn't exist"
        Just (state, code) -> if state /= Taken
                              then Right code
                              else Left $ "Locker" ++ show lockerNumber ++ "is already taken" 


infixr :-:
data List a = Empty | a :-: (List a) 
            deriving (Show, Read, Eq, Ord)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving( Show )

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

data Point = Point Float Float deriving( Show )

move' :: Point -> Float -> Float -> Point
move' (Point x y) disx disy = Point (x+disx) (y+disy)

data Person = Person {
    firstName :: String, 
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String    
} deriving (Show)

data Vector a = Vector a a a deriving (Show)

-- vectorPlus' :: Vector a -> Vector a -> Vector a
vectorPlus' (Vector a1 b1 c1) (Vector a2 b2 c2) = Vector (a1+a2) (b1+b2) (c1+c2)