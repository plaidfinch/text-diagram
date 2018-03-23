module Text.Graphics.BoxDrawing.Coordinate
  ( X
  , Y
  , Point
  , Move(..)
  , Distance(..)
  ) where

import Data.Ix

import Text.Graphics.BoxDrawing.Cardinal

type Point = (X, Y)

newtype X
  = X Integer
  deriving (Eq, Ord, Num, Enum, Integral, Real, Ix)
  deriving newtype (Show)

newtype Y
  = Y Integer
  deriving (Eq, Ord, Num, Enum, Integral, Real, Ix)
  deriving newtype (Show)

class Move p d where
  move :: Integer -> Cardinal d -> p -> p

instance Move X Horizontal where
  move n L (X x) = (X (x - n))
  move n R (X x) = (X (x + n))

instance Move Y Vertical where
  move n U (Y y) = Y (y - n)
  move n D (Y y) = Y (y + n)

instance Move (X, Y) Horizontal where
  move n d (x, y) = (move n d x, y)

instance Move (X, Y) Vertical where
  move n d (x, y) = (x, move n d y)

class Distance p where
  distance :: Num n => p -> p -> n

instance Distance X where
  (X a) `distance` (X b) = fromInteger (b - a)

instance Distance Y where
  (Y a) `distance` (Y b) = fromInteger (b - a)
