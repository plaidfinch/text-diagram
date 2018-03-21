module Text.Graphics.BoxDrawing.Coordinate
  ( X
  , Y
  , Coordinate
  , c
  , locate
  ) where

newtype X
  = X Integer
  deriving (Eq, Ord, Num)
  deriving newtype (Show)

newtype Y
  = Y Integer
  deriving (Eq, Ord, Num)
  deriving newtype (Show)

data Coordinate
  = Coordinate ((?width::X) => X) ((?height::Y) => Y)

c :: ((?width::X) => X) -> ((?height::Y) => Y) -> Coordinate
c = Coordinate

locate :: X -> Y -> Coordinate -> Maybe (X, Y)
locate width height (Coordinate x y) =
  let ?width  = width
      ?height = height
  in if 0 <= x && x <= ?width
     && 0 <= y && y <= ?height
     then Just (x, y)
     else Nothing
