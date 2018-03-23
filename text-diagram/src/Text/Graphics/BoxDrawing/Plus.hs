module Text.Graphics.BoxDrawing.Plus where

import Data.Semigroup
import Data.Universe
import Control.Applicative
import Data.List

import Text.Graphics.BoxDrawing.Style
import Text.Graphics.BoxDrawing.Cardinal

-- ----------------
-- Semantic symbols
-- ----------------

-- Semantic representation of box-drawing cells
-- Behaves like a strict quadruple, accessed via Cardinals

newtype Plus a
  = Plus (forall d. Cardinal d -> a)

type Plus' = Plus Style'

(@@) :: Plus a -> Cardinal d -> a
(@@) (Plus f) = f

makePlus :: a -> a -> a -> a -> Plus a
makePlus !u !l !r !d =
  Plus $ cardinal u l r d

instance Eq a => Eq (Plus a) where
  Plus p == Plus q =
    onCardinals p == onCardinals q

instance Ord a => Ord (Plus a) where
  Plus p `compare` Plus q =
    onCardinals p `compare` onCardinals q

instance Universe a => Universe (Plus a) where
  universe =
    makePlus
      <$> universe
      <*> universe
      <*> universe
      <*> universe

instance Finite a => Finite (Plus a)

instance Functor Plus where
  fmap f (Plus g) =
    makePlus
      (f (g U))
      (f (g L))
      (f (g R))
      (f (g D))

instance Applicative Plus where
  pure x = Plus (const x)
  Plus f <*> Plus g =
    makePlus
      (f U (g U))
      (f L (g L))
      (f R (g R))
      (f D (g D))

instance Semigroup a => Semigroup (Plus a) where
  f <> g = liftA2 (<>) f g

instance Monoid a => Monoid (Plus a) where
  mempty  = pure mempty
  mappend = liftA2 mappend

instance Foldable Plus where
  foldMap f p =
    mconcat (onCardinals (fmap f p @@))

instance Traversable Plus where
  sequenceA (Plus f) =
    makePlus <$> f U <*> f L <*> f R <*> f D

instance Show a => Show (Plus a) where
  showsPrec d !(Plus f) =
    showParen (d > 10) . showString $
      "makePlus "
      ++ intercalate " "
           (map (($ []) . showsPrec 11) (onCardinals f))
