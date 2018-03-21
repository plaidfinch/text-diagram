module Text.Graphics.BoxDrawing.Cardinal where

-- -------------------
-- Cardinal directions
-- -------------------

data Axis
  = Vertical
  | Horizontal
  deriving (Eq, Ord, Show)

data Cardinal (a :: Axis) where
  U :: Cardinal Vertical
  L :: Cardinal Horizontal
  R :: Cardinal Horizontal
  D :: Cardinal Vertical

deriving instance Show (Cardinal d)
deriving instance Ord  (Cardinal d)
deriving instance Eq   (Cardinal d)

cardinal :: a -> a -> a -> a -> Cardinal d -> a
cardinal !u !l !r !d =
  \case
    U -> u
    L -> l
    R -> r
    D -> d

onCardinals :: (forall d. Cardinal d -> a) -> [a]
onCardinals f = [f U, f L, f R, f D]
