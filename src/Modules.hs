import Text.Read (readMaybe)
import System.IO
import Data.List
import Geometry.Sphere as Sphere
import Geometry.Cuboid as Cuboid
import Geometry.Cube as Cube
-- if we only needed nub and sort we can do:
-- import Data.List (nub, sort)
--
-- otherwise - if we want EVERYTHING EXCEPT nub we can do:
-- import Data.List hiding (nub)
--
-- Lastly, if we have 2 conflicting module names, we can force 
-- using the full path using the qualified keyword or giving 
-- a nickname such as either of these
-- import qualified Data.List
--   this would be Data.List.nub
-- import qualified Data.List as M
--   this would be M.nub

numsUniques :: (Eq a) => [a] -> Int
numsUniques = length . nub

data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

-- Record syntax
data Person = Person { firstName :: String
        , lastName :: String
        , age :: Int
        , height :: Float
        , phoneNumber :: String
        , flavor :: String
        } deriving (Show)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
singleton' :: a -> Tree a
singleton' x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton' x
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


