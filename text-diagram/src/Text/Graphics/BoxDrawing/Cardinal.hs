module Text.Graphics.BoxDrawing.Cardinal where

-- -------------------
-- Cardinal directions
-- -------------------

data Axis
  = Vertical
  | Horizontal
  deriving (Eq, Ord, Show)

type family Rotate (a :: Axis) :: Axis where
  Rotate Vertical   = Horizontal
  Rotate Horizontal = Vertical

data Cardinal (a :: Axis) where
  U :: Cardinal Vertical
  L :: Cardinal Horizontal
  R :: Cardinal Horizontal
  D :: Cardinal Vertical

deriving instance Show (Cardinal d)
deriving instance Ord  (Cardinal d)
deriving instance Eq   (Cardinal d)

opposite :: Cardinal d -> Cardinal d
opposite U = D
opposite D = U
opposite L = R
opposite R = L

clockwise :: Cardinal d -> Cardinal (Rotate d)
clockwise U = R
clockwise R = D
clockwise D = L
clockwise L = U

anticlockwise :: Cardinal d -> Cardinal (Rotate d)
anticlockwise U = L
anticlockwise L = D
anticlockwise D = R
anticlockwise R = U

axis :: Cardinal d -> Axis
axis U = Vertical
axis L = Horizontal
axis R = Horizontal
axis D = Vertical

cardinal :: a -> a -> a -> a -> Cardinal d -> a
cardinal !u !l !r !d =
  \case
    U -> u
    L -> l
    R -> r
    D -> d

onCardinals :: (forall d. Cardinal d -> a) -> [a]
onCardinals f = [f U, f L, f R, f D]
