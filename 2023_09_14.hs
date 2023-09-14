import Data.List  
import Data.Int (Int8)

import qualified Geometry as G

numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub

myVol :: Float -> Float
myVol = G.cubeVolume . G.sphereVolume 


{- Types -}
data Point = Point {
  x:: Float,
  y:: Float
} deriving (Show)

data Circle = Circle {
  center:: Point,
  radius:: Float
}deriving (Show)

data Rectangle = Rectangle {
  topLeft:: Point,
  botRight:: Point
} deriving (Show)

data Shape = ShapeCircle Circle | ShapeRectangle Rectangle

area :: Shape -> Float
area (ShapeCircle (Circle _ r)) = pi * r * r
area (ShapeRectangle (Rectangle (Point x1 y1) (Point x2 y2))) = abs (x2 - x1) * abs (y2 - y1)

{- usage
 -
let centerPoint = Point { x = 0, y = 0 }
let myCircle = Circle { center = centerPoint, radius = 5 }
let circleShape = ShapeCircle myCircle
area circleShape
-
-}

-- Or 
--

data Shape2 = Circle2 {
  center2:: Point,
  radius2:: Float
} | Rectangle2 {
  topLeft2:: Point,
  botRight2:: Point
}

area2 :: Shape2 -> Float
area2 (Circle2 _ r) = pi * r * r
area2 (Rectangle2 (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

-- Or
data Car a b = Car {
  brand:: a,
  model:: b
} deriving(Show)

data Hardware = Hammer {
  hwbrand:: String,
  value:: Float
} |  Screw {
  screwid:: String,
  screwtype:: Maybe String
}



data Rank =
      Rank2
    | Rank3
    | Rank4
    | Rank5
    | Rank6
    | Rank7
    | Rank8
    | Rank9
    | Rank10
    | Jack
    | Queen
    | King
    | Ace
    deriving (Bounded, Enum, Show)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving(Show, Read, Eq)

singleNode :: (Ord a) => a -> Tree a
singleNode a = Node a EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert a EmptyTree = singleNode a
treeInsert a (Node x left right)
  | a == x = Node x left right
  | a < x = Node x (treeInsert a left) right
  | a > x = Node x left (treeInsert a right)

isInTree :: (Ord a) => a -> Tree a -> Bool
isInTree x EmptyTree = False
isInTree x (Node a left right)
  | a == x = True
  | x > a = isInTree x right
  | x < a = isInTree x left

