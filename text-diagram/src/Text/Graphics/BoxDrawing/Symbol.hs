{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms, DerivingStrategies,
             GeneralizedNewtypeDeriving, LambdaCase, BangPatterns,
             StandaloneDeriving, UndecidableInstances, GADTs, RankNTypes,
             FlexibleInstances, ViewPatterns, ScopedTypeVariables, DataKinds,
             KindSignatures, NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -Wall -fno-warn-unticked-promoted-constructors #-}

module Text.Graphics.BoxDrawing.Symbol
  ( Thickness(..)
  , LineStyle(..)
  , Style(..), Style'
  , Axis(..)
  , Cardinal(..), cardinal, onCardinals
  , Plus(..), Plus', (@@), makePlus
  , Symbol, getSymbol, makeSymbol, exactSymbol, approxSymbol, parseSymbol
  ) where

import Data.Semigroup
import Control.Applicative
import Data.String
import Data.Universe
import Data.Bits
import Data.Maybe
import Data.List
import Data.Foldable
import Data.Traversable

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


-- -------------------
-- Cardinal directions
-- -------------------

data Axis = Vertical | Horizontal

data Cardinal (a :: Axis) where
  U :: Cardinal Vertical
  L :: Cardinal Horizontal
  R :: Cardinal Horizontal
  D :: Cardinal Vertical

cardinal :: a -> a -> a -> a -> Cardinal d -> a
cardinal !u !l !r !d =
  \case
    U -> u
    L -> l
    R -> r
    D -> d

onCardinals :: (forall d. Cardinal d -> a) -> [a]
onCardinals f = [f U, f L, f R, f D]

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


-- -----------------
-- Syntactic symbols
-- -----------------

-- Box drawing characters, restricted to those represented by our framework
-- We don't use the entire Unicode block, and you can't construct unused ones

newtype Symbol
  = Symbol Char
  deriving (Eq, Ord)

getSymbol :: Symbol -> Char
getSymbol (Symbol c) = c

instance Show Symbol where
  showsPrec n (Symbol c) =
    showParen (n > 10) . showString $ "Symbol '" ++ [c] ++ "'"

boxCharNum :: Symbol -> Int
boxCharNum (Symbol c)
  | c == ' '  = 128
  | otherwise = fromEnum c - 0x2500

unsafeSymbol :: Int -> Symbol
unsafeSymbol i
  | i == 128 = Symbol ' '
  | i >= 0 && i < 128 = Symbol (toEnum (i + 0x2500))
  | otherwise = error "unsafeSymbol: out of range"


-- -------------------------------
-- Sets of symbols (internal only)
-- -------------------------------

-- Sets of Symbols, efficiently packed as bit-vectors
-- There are potentially 129 (yes, I know) characters:
-- 128 from the unicode block + the space character ' '

data SymbolSet
  = SS !Word !Word !Bool

instance Show SymbolSet where
  showsPrec n bcs =
    showParen (n > 10) . showString $
      "fromString \"" ++ map getSymbol (listSS bcs) ++ "\""

emptySS :: SymbolSet
emptySS = SS 0 0 False

insertSS :: Symbol -> SymbolSet -> SymbolSet
insertSS (boxCharNum -> c) (SS small large space)
  | c < 64    = SS (setBit small c) large space
  | c < 128   = SS small (setBit large (c - 64)) space
  | otherwise = SS small large True

instance Semigroup SymbolSet where
  (<>) = intersectSS

instance Monoid SymbolSet where
  mempty  = allSymbols
  mappend = (<>)

memberSS :: Symbol -> SymbolSet -> Bool
memberSS (boxCharNum -> c) (SS small large space)
  | c < 64    = testBit small c
  | c < 128   = testBit large (c - 64)
  | otherwise = space

intersectSS :: SymbolSet -> SymbolSet -> SymbolSet
intersectSS (SS small1 large1 space1) (SS small2 large2 space2) =
  SS (small1 .&. small2) (large1 .&. large2) (space1 && space2)

unionSS :: SymbolSet -> SymbolSet -> SymbolSet
unionSS (SS small1 large1 space1) (SS small2 large2 space2) =
  SS (small1 .|. small2) (large1 .|. large2) (space1 || space2)

makeSymbol :: Char -> Maybe Symbol
makeSymbol c@(subtract 0x2500 . fromEnum -> i)
  | c == ' ' = Just (Symbol ' ')
  | i >= 0 && i < 128 =
    let bc = unsafeSymbol i
    in if bc `memberSS` allSymbols
       then Just bc
       else Nothing
  | otherwise = Nothing

makeSS :: String -> SymbolSet
makeSS = foldr insertSS emptySS . mapMaybe makeSymbol

unsafeMakeSS :: String -> SymbolSet
unsafeMakeSS =
  foldr insertSS emptySS
  . map Symbol
  . filter (\c@(subtract 0x2500 . fromEnum -> i) ->
              c == ' ' || (0 <= i && i < 128))

instance IsString SymbolSet where
  fromString = makeSS

listSS :: SymbolSet -> [Symbol]
listSS (SS small large space) =
  map unsafeSymbol $
    trueBits small
    ++ map (+ 64) (trueBits large)
    ++ if space then [128] else []
  where
    trueBits b = filter (testBit b) [0..63]

singleSS :: SymbolSet -> Maybe (Maybe Symbol)
singleSS (SS 0 0 False) = Just Nothing
singleSS (SS 0 0 True)  = Just (Just (Symbol ' '))
singleSS (SS 0 l False)
  | popCount l == 1
  = Just (Just (unsafeSymbol (64 + countTrailingZeros l)))
singleSS (SS s 0 False)
  | popCount s == 1
  = Just (Just (unsafeSymbol (countTrailingZeros s)))
singleSS _ = Nothing


-- ----------------------------
-- Manipulating sets of symbols
-- ----------------------------

-- All the allowable symbols, indexed by style and cardinal

symbols :: Plus (Style' -> SymbolSet)
symbols =
  Plus $ flip $ \case
    Transparent         -> transparent
    White               -> transparent
    Line (Solid Thin)   -> solidThin
    Line (Solid Thick)  -> solidThick
    Line (Dashed Thin)  -> dashedThin
    Line (Dashed Thick) -> dashedThick
    Line Double         -> double
  where
    transparent = make u l r d
      where
        u = " ╴╸╶╺╷╻─━╼╾┌┍┎┏┐┑┒┓┬┭┮┯┰┱┲┳╌╍═╒╓╔╕╖╗╤╥╦"
        l = " ╵╹╶╺╷╻│┃╽╿┌┍┎┏└┕┖┗├┝┞┟┠┡┢┣╎╏║╒╓╔╘╙╚╞╟╠"
        r = " ╵╹╴╸╷╻│┃╽╿┐┑┒┓┘┙┚┛┤┥┦┧┨┩┪┫╎╏║╕╖╗╛╜╝╡╢╣"
        d = " ╵╹╴╸╶╺─━╼╾└┕┖┗┘┙┚┛┴┵┶┷┸┹┺┻╌╍═╘╙╚╛╜╝╧╨╩"
    solidThin = make u l r d
      where
        u = "╵│╽└┕┘┙├┝┟┢┤┥┧┪┴┵┶┷┼┽┾┿╁╅╆╈╘╛╞╡╧╪"
        l = "╴─╼┐┒┘┚┤┦┧┨┬┮┰┲┴┶┸┺┼┾╀╁╂╄╆╊╖╜╢╥╨╫"
        r = "╶─╾┌┎└┖├┞┟┠┬┭┰┱┴┵┸┹┼┽╀╁╂╃╅╉╓╙╟╥╨╫"
        d = "╷│╿┌┍┐┑├┝┞┡┤┥┦┩┬┭┮┯┼┽┾┿╀╃╄╇╒╕╞╡╤╪"
    solidThick = make u l r d
      where
        u = "╹┃╿┖┗┚┛┞┠┡┣┦┨┩┫┸┹┺┻╀╂╃╄╇╉╊╋"
        l = "╸━╾┑┓┙┛┥┩┪┫┭┯┱┳┵┷┹┻┽┿╃╅╇╉╈╋"
        r = "╺━╼┍┏┕┗┝┡┢┣┮┯┲┳┶┷┺┻┾┿╄╆╇╈╊╋"
        d = "╻┃╽┎┏┒┓┟┠┢┣┧┨┪┫┰┱┲┳╁╂╅╆╈╉╊╋"
    dashedThin = make u l r d
      where
        u = "╎"
        l = "╌"
        r = "╌"
        d = "╎"
    dashedThick = make u l r d
      where
        u = "╏"
        l = "╍"
        r = "╍"
        d = "╏"
    double = make u l r d
      where
        u = "║╙╚╜╝╟╠╢╣╨╩╫╬"
        l = "═╕╗╛╝╡╣╤╦╧╩╪╬"
        r = "═╒╔╘╚╞╠╤╦╧╩╪╬"
        d = "║╓╔╖╗╟╠╢╣╥╦╫╬"
    make u l r d =
      cardinal
        (unsafeMakeSS u)
        (unsafeMakeSS l)
        (unsafeMakeSS r)
        (unsafeMakeSS d)

symbolSet :: Cardinal d -> Style' -> SymbolSet
symbolSet = (@@) symbols

allSymbols :: SymbolSet
allSymbols =
  foldr unionSS emptySS $
    onCardinals (\d -> foldr unionSS emptySS
                         (map (symbolSet d) universe))

exactSymbol :: Plus' -> Maybe Symbol
exactSymbol plus =
  let intersection =
        fold (symbols <*> plus)
  in fromMaybe
       (error $ "getSymbol: non-unique result (should be impossible): "
                ++ show intersection)
       (singleSS intersection)

approxSymbol :: Plus' -> Symbol
approxSymbol plus =
  fromMaybe
    (error $ "approxSymbol: no result (should be impossible): " ++ show plus)
    (asum [ exactSymbol plus
          , exactSymbol (removeDashed plus)
          , exactSymbol (removeDouble . removeDashed $ plus) ])
  where
    removeDashed =
      fmap $ \case
        Line (Dashed t) -> Line (Solid t)
        s -> s
    removeDouble =
      fmap $ \case
        Line Double -> Line (Solid Thick)
        s -> s

parseSymbol :: Symbol -> Plus'
parseSymbol c =
  fromMaybe (error "parseSymbol: symbol not found (should be impossible)") $
    for symbols $ \f ->
      find (memberSS c . f) universe


-- -------------------
-- Instances of things
-- -------------------

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

-- Symbol

instance Semigroup Symbol where
  c <> d = approxSymbol (parseSymbol c <> parseSymbol d)

instance Monoid Symbol where
  mempty = Symbol ' '
  mappend = (<>)

instance Universe Symbol where
  universe = listSS allSymbols

instance Finite Symbol

-- Plus

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

-- Cardinal

deriving instance Show (Cardinal d)
deriving instance Ord  (Cardinal d)
deriving instance Eq   (Cardinal d)
