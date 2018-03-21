
module Text.Graphics.BoxDrawing.Style where

import Data.Semigroup
import Data.Universe

-- -----------
-- Line styles
-- -----------

data Thickness
  = Thin
  | Thick
  deriving (Show, Eq, Ord)

data LineStyle
  = Dashed !Thickness
  | Solid  !Thickness
  | Double
  deriving (Show, Eq, Ord)

data Style l
  = Transparent
  | Line !l
  | White
  deriving (Show, Eq, Ord)

type Style' = Style LineStyle

-- ---------
-- Instances
-- ---------

-- Thickness

instance Semigroup Thickness where
  Thin  <> t     = t
  t     <> Thin  = t
  Thick <> Thick = Thick

instance Monoid Thickness where
  mempty  = Thin
  mappend = (<>)

instance Universe Thickness where
  universe = [Thin, Thick]

instance Finite Thickness

--LineStyle

instance Semigroup LineStyle where
  Dashed s <> Dashed t = Dashed (s <> t)
  Dashed s <> Solid  t = Solid  (s <> t)
  Solid  s <> Dashed t = Solid  (s <> t)
  Solid  s <> Solid  t = Solid  (s <> t)
  Double   <> _        = Double
  _        <> Double   = Double

instance Monoid LineStyle where
  mempty  = Dashed mempty
  mappend = (<>)

instance Universe LineStyle where
  universe = Double : ([Dashed, Solid] <*> universe)

instance Finite LineStyle

-- Style

instance Semigroup (Style l) where
  Transparent <> s = s
  s <> Transparent = s
  _ <> s = s

instance Monoid (Style l) where
  mempty  = Transparent
  mappend = (<>)

instance Universe l => Universe (Style l) where
  universe = Transparent : White : (Line <$> universe)

instance Finite l => Finite (Style l)
