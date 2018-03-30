module Text.Graphics.BoxDrawing.Canvas where

import Control.Monad.ST
import Data.Array.ST
import Control.Monad
import Data.List
import Data.Semigroup
import Control.Monad.Reader

import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import Text.Graphics.BoxDrawing.Symbol
import Text.Graphics.BoxDrawing.Coordinate
import Text.Graphics.BoxDrawing.Style
import Text.Graphics.BoxDrawing.Cardinal
import Text.Graphics.BoxDrawing.Plus

data Character
  = C !Char
  | S !Plus'
  deriving (Eq, Ord, Show)

character :: Character -> Char
character (C c) = c
character (S s) = getSymbol (approxSymbol s)

instance Semigroup Character where
  C _ <> c@(C _) = c
  C c <> S t
    | t == mempty = C c
    | otherwise   = S t
  S _ <> c@(C _) = c
  S s <> S t     = S (s <> t)

instance Monoid Character where
  mempty  = S mempty
  mappend = (<>)

type Pen m a = Point -> a -> m ()

newtype Drawing a =
  Draw (forall m. Monad m => ReaderT (Pen m a) m ())

draw :: Monad m => Drawing a -> ReaderT (Pen m a) m ()
draw (Draw drawing) = drawing

instance Semigroup (Drawing a) where
  Draw d <> Draw e = Draw (d >> e)

instance Monoid (Drawing a) where
  mempty = Draw (return ())
  mappend = (<>)

type Drawing' = Drawing Character

pen :: Monad m => Point -> a -> ReaderT (Pen m a) m ()
pen point a = do
  p <- ask
  lift (p point a)

stroke
  :: Move (X, Y) d
  => Style' -> Point -> Maybe (Cardinal d) -> Cardinal d -> Rational -> Drawing'
stroke style (x, y) startDir dir dist
  | dist == 0 = mempty
  | dist <  0 =
      stroke style (x, y) startDir (opposite dir) (negate dist)
  | otherwise =
      case startDir of
        Nothing ->
          Draw (pen (x, y) (S (segment style dir))) <> next (1/2)
        Just start
          | start == dir -> next 1
          | dist >= 1 ->
              Draw (pen (x, y) (S (across style (axis dir)))) <> next 1
          | otherwise ->
              Draw (pen (x, y) (S (segment style (opposite dir))))
      where
        next n =
          stroke style
            (move 1 dir (x, y))
            (Just (opposite dir))
            dir
            (dist - n)

box :: Style' -> Point -> Point -> Drawing'
box style (l, t) (r, b) =
  mconcat
    [ stroke style (l, t) Nothing R (l `distance` r)
    , stroke style (l, t) Nothing D (t `distance` b)
    , stroke style (r, b) Nothing L (l `distance` r)
    , stroke style (r, b) Nothing U (t `distance` b)
    ]

fill :: Style' -> Point -> Point -> Drawing'
fill style (l, t) (r, b) =
  mconcat
    [ stroke style (l, row) Nothing R (l `distance` r)
    | row <- range (t+1, b-1)
    ]
  <>
  mconcat
    [ stroke style (col, t) Nothing D (t `distance` b)
    | col <- range (l+1, r-1)
    ]

text :: Point -> Bool -> String -> Drawing'
text (x, y) opaque str = Draw $
  forM_ (zip [0..] (lines str)) $
    \((y +) -> row, line) ->
      forM_ (zip [0..] line) $
        \((x +) -> col, c) ->
          if opaque || c /= ' '
          then pen (col, row) (C c)
          else return ()

newtype Canvas array a
  = Canvas (array (X, Y) a)

blank
  :: (MArray array a m, Monoid a)
  => Point -> m (Canvas array a)
blank (width, height) =
  Canvas <$> newArray ((1, 1), (width, height)) mempty

canvasSize :: MArray array a m => Canvas array a -> m Point
canvasSize (Canvas canvas) = snd <$> getBounds canvas

drawOn
  :: (Monoid a, MArray array a m)
  => Canvas array a -> Drawing a -> m ()
drawOn c@(Canvas canvas) drawing = do
  (width, height) <- canvasSize c
  runReaderT (draw drawing) (write width height)
  where
    write width height (x, y) a
      |  0 < x && x <= width && 0 < y && y <= height
      = do old <- readArray canvas (x, y)
           writeArray canvas (x, y) (old `mappend` a)
      | otherwise
      = return ()

render
  :: (Monoid a, MArray array a m)
  => (a -> Char) -> Canvas array a -> m Text
render perChar c@(Canvas canvas) =
  T.pack <$> intercalate "\n"
         <$> map (map perChar)
         <$> getRows
  where
    getRows = do
      (width, height) <- canvasSize c
      forM (range (1,height)) $ \y ->
        forM (range (1,width)) $ \x ->
          readArray canvas (x, y)

renderOut
  :: (Monoid a, MArray array a IO)
  => (a -> Char) -> Canvas array a -> IO ()
renderOut perChar canvas =
  T.putStrLn =<< render perChar canvas

sketch :: forall a. Monoid a => (a -> Char) -> Point -> Drawing a -> Text
sketch perChar dimensions drawing =
  runST $ do
    canvas <- blank dimensions :: ST s (Canvas (STArray s) a)
    drawOn canvas drawing
    render perChar canvas

sketchOut :: Monoid a => (a -> Char) -> Point -> Drawing a -> IO ()
sketchOut perChar dimensions drawing =
  T.putStrLn (sketch perChar dimensions drawing)
