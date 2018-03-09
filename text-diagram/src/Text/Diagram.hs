{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms, DerivingStrategies,
             GeneralizedNewtypeDeriving, LambdaCase, BangPatterns,
             StandaloneDeriving, UndecidableInstances, GADTs, RankNTypes,
             FlexibleInstances, ViewPatterns, ScopedTypeVariables, DataKinds,
             KindSignatures #-}

{-# OPTIONS_GHC -Wall #-}

module Text.Graphics where
-- module Main where

import Data.Semigroup
import Control.Applicative
import Data.String
import Test.QuickCheck hiding ((.&.))
import Data.Universe
import Data.Bits
import Data.Maybe
import Data.List
import Data.Foldable
import Data.Traversable


-- main :: IO ()
-- main = putStrLn $ mapMaybe (fmap getBoxChar . symbol) universe

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

data Axis = Vertical | Horizontal

data Cardinal (a :: Axis) where
  U :: Cardinal Vertical
  L :: Cardinal Horizontal
  R :: Cardinal Horizontal
  D :: Cardinal Vertical

newtype Plus a
  = Plus (forall d. Cardinal d -> a)

getSegment :: Plus a -> Cardinal d -> a
getSegment (Plus f) = f

makePlus :: a -> a -> a -> a -> Plus a
makePlus !u !l !r !d =
  Plus $ \case
    U -> u
    L -> l
    R -> r
    D -> d

newtype BoxChar
  = BoxChar Char
  deriving (Eq, Ord)

getBoxChar :: BoxChar -> Char
getBoxChar (BoxChar c) = c

instance Show BoxChar where
  showsPrec n (BoxChar c) =
    showParen (n > 10) . showString $ "BoxChar '" ++ [c] ++ "'"

boxCharNum :: BoxChar -> Int
boxCharNum (BoxChar c)
  | c == ' '  = 128
  | otherwise = fromEnum c - 0x2500

unsafeBoxChar :: Int -> BoxChar
unsafeBoxChar i
  | i == 128 = BoxChar ' '
  | i >= 0 && i < 128 = BoxChar (toEnum (i + 0x2500))
  | otherwise = error "unsafeBoxChar: out of range"

data BoxCharSet
  = BCS !Word !Word !Bool

instance Show BoxCharSet where
  showsPrec n bcs =
    showParen (n > 10) . showString $
      "fromString \"" ++ map getBoxChar (listBCS bcs) ++ "\""

emptyBCS :: BoxCharSet
emptyBCS = BCS 0 0 False

insertBCS :: BoxChar -> BoxCharSet -> BoxCharSet
insertBCS (boxCharNum -> c) (BCS small large space)
  | c < 64    = BCS (setBit small c) large space
  | c < 128   = BCS small (setBit large (c - 64)) space
  | otherwise = BCS small large True

singletonBCS :: BoxChar -> BoxCharSet
singletonBCS c = insertBCS c emptyBCS

instance Semigroup BoxCharSet where
  (<>) = intersectBCS

instance Monoid BoxCharSet where
  mempty  = BCS maxBound maxBound True
  mappend = (<>)

memberBCS :: BoxChar -> BoxCharSet -> Bool
memberBCS (boxCharNum -> c) (BCS small large space)
  | c < 64    = testBit small c
  | c < 128   = testBit large (c - 64)
  | otherwise = space

intersectBCS :: BoxCharSet -> BoxCharSet -> BoxCharSet
intersectBCS (BCS small1 large1 space1) (BCS small2 large2 space2) =
  BCS (small1 .&. small2) (large1 .&. large2) (space1 && space2)

unionBCS :: BoxCharSet -> BoxCharSet -> BoxCharSet
unionBCS (BCS small1 large1 space1) (BCS small2 large2 space2) =
  BCS (small1 .|. small2) (large1 .|. large2) (space1 || space2)

boxChar :: Char -> Maybe BoxChar
boxChar c@(subtract 0x2500 . fromEnum -> i)
  | c == ' ' = Just (unsafeBoxChar 128)
  | i >= 0 && i < 128 =
    let bc = unsafeBoxChar i
    in if bc `memberBCS` allSymbols
       then Just bc
       else Nothing
  | otherwise = Nothing

makeBCS :: String -> BoxCharSet
makeBCS = foldr insertBCS emptyBCS . mapMaybe boxChar

unsafeMakeBCS :: String -> BoxCharSet
unsafeMakeBCS =
  foldr insertBCS emptyBCS
  . map BoxChar
  . filter (\c@(subtract 0x2500 . fromEnum -> i) ->
              c == ' ' || (0 <= i && i < 128))

instance IsString BoxCharSet where
  fromString = makeBCS

listBCS :: BoxCharSet -> [BoxChar]
listBCS (BCS small large space) =
  map unsafeBoxChar $
    trueBits small
    ++ map (+ 64) (trueBits large)
    ++ if space then [128] else []
  where
    trueBits b = filter (testBit b) [0..63]

singleBCS :: BoxCharSet -> Maybe (Maybe BoxChar)
singleBCS (BCS 0 0 False) = Just Nothing
singleBCS (BCS 0 0 True)  = Just (Just (unsafeBoxChar 128))
singleBCS (BCS 0 l False)
  | popCount l == 1
  = Just (Just (unsafeBoxChar (64 + countTrailingZeros l)))
singleBCS (BCS s 0 False)
  | popCount s == 1
  = Just (Just (unsafeBoxChar (countTrailingZeros s)))
singleBCS _ = Nothing

symbols :: Plus (Style LineStyle -> BoxCharSet)
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
    transparent, solidThin, solidThick, dashedThin, dashedThick, double
      :: Cardinal d -> BoxCharSet
    transparent =
      \case
        U -> unsafeMakeBCS " ╴╸╶╺╷╻─━╼╾┌┍┎┏┐┑┒┓┬┭┮┯┰┱┲┳╌╍═╒╓╔╕╖╗╤╥╦"
        L -> unsafeMakeBCS " ╵╹╶╺╷╻│┃╽╿┌┍┎┏└┕┖┗├┝┞┟┠┡┢┣╎╏║╒╓╔╘╙╚╞╟╠"
        R -> unsafeMakeBCS " ╵╹╴╸╷╻│┃╽╿┐┑┒┓┘┙┚┛┤┥┦┧┨┩┪┫╎╏║╕╖╗╛╜╝╡╢╣"
        D -> unsafeMakeBCS " ╵╹╴╸╶╺─━╼╾└┕┖┗┘┙┚┛┴┵┶┷┸┹┺┻╌╍═╘╙╚╛╜╝╧╨╩"
    solidThin =
      \case
        U -> unsafeMakeBCS "╵│╽└┕┘┙├┝┟┢┤┥┧┪┴┵┶┷┼┽┾┿╁╅╆╈╘╛╞╡╧╪"
        L -> unsafeMakeBCS "╴─╼┐┒┘┚┤┦┧┨┬┮┰┲┴┶┸┺┼┾╀╁╂╄╆╊╖╜╢╥╨╫"
        R -> unsafeMakeBCS "╶─╾┌┎└┖├┞┟┠┬┭┰┱┴┵┸┹┼┽╀╁╂╃╅╉╓╙╟╥╨╫"
        D -> unsafeMakeBCS "╷│╿┌┍┐┑├┝┞┡┤┥┦┩┬┭┮┯┼┽┾┿╀╃╄╇╒╕╞╡╤╪"
    solidThick =
      \case
        U -> unsafeMakeBCS "╹┃╿┖┗┚┛┞┠┡┣┦┨┩┫┸┹┺┻╀╂╃╄╇╉╊╋"
        L -> unsafeMakeBCS "╸━╾┑┓┙┛┥┩┪┫┭┯┱┳┵┷┹┻┽┿╃╅╇╉╈╋"
        R -> unsafeMakeBCS "╺━╼┍┏┕┗┝┡┢┣┮┯┲┳┶┷┺┻┾┿╄╆╇╈╊╋"
        D -> unsafeMakeBCS "╻┃╽┎┏┒┓┟┠┢┣┧┨┪┫┰┱┲┳╁╂╅╆╈╉╊╋"
    dashedThin =
      \case
        U -> unsafeMakeBCS "╎"
        L -> unsafeMakeBCS "╌"
        R -> unsafeMakeBCS "╌"
        D -> unsafeMakeBCS "╎"
    dashedThick =
      \case
        U -> unsafeMakeBCS "╏"
        L -> unsafeMakeBCS "╍"
        R -> unsafeMakeBCS "╍"
        D -> unsafeMakeBCS "╏"
    double=
      \case
        U -> unsafeMakeBCS "║╙╚╜╝╟╠╢╣╨╩╫╬"
        L -> unsafeMakeBCS "═╕╗╛╝╡╣╤╦╧╩╪╬"
        R -> unsafeMakeBCS "═╒╔╘╚╞╠╤╦╧╩╪╬"
        D -> unsafeMakeBCS "║╓╔╖╗╟╠╢╣╥╦╫╬"

symbolSet :: Cardinal d -> Style LineStyle -> BoxCharSet
symbolSet = getSegment symbols

allSymbols :: BoxCharSet
allSymbols =
  foldr unionBCS emptyBCS $
    onCardinals (\d -> foldr unionBCS emptyBCS
                         (map (symbolSet d) universe))

onCardinals :: (forall d. Cardinal d -> a) -> [a]
onCardinals f = [f U, f L, f R, f D]

symbol :: Plus (Style LineStyle) -> Maybe BoxChar
symbol plus =
  let intersection =
        fold (symbols <*> plus)
  in fromMaybe
       (error $ "getSymbol: non-unique result (should be impossible): "
                ++ show intersection)
       (singleBCS intersection)

approxSymbol :: Plus (Style LineStyle) -> BoxChar
approxSymbol plus =
  fromMaybe
    (error $ "approxSymbol: no result (should be impossible): " ++ show plus)
    (symbol plus
     <|> symbol (removeDashed plus)
     <|> symbol (removeDouble . removeDashed $ plus))
  where
    removeDashed =
      fmap $ \case
        Line (Dashed t) -> Line (Solid t)
        s               -> s
    removeDouble =
      fmap $ \case
        Line Double -> Line (Solid Thin)
        s           -> s

parseSymbol :: BoxChar -> Plus (Style LineStyle)
parseSymbol c =
  fromMaybe (error "parseSymbol: symbol not found (should be impossible)") $
    for symbols $ \f ->
      find (memberBCS c . f) universe

-- Instances of things

instance Semigroup Thickness where
  Thin  <> t     = t
  t     <> Thin  = t
  Thick <> Thick = Thick

instance Monoid Thickness where
  mempty  = Thin
  mappend = (<>)

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

instance Semigroup (Style l) where
  Transparent <> s = s
  s <> Transparent = s
  _ <> s = s

instance Monoid (Style l) where
  mempty  = Transparent
  mappend = (<>)

instance Arbitrary Thickness where
  arbitrary = elements [Thin, Thick]

instance Arbitrary LineStyle where
  arbitrary =
    frequency [ (1, return Double)
              , (2, Solid  <$> arbitrary)
              , (2, Dashed <$> arbitrary) ]

instance Arbitrary l => Arbitrary (Style l) where
  arbitrary =
    frequency [ (1, return Transparent)
              , (4, Line <$> arbitrary)
              , (1, return White) ]

instance Arbitrary BoxChar where
  arbitrary =
    elements (listBCS allSymbols)

instance Universe BoxChar where
  universe = listBCS allSymbols

instance Finite BoxChar

instance Semigroup BoxChar where
  c <> d = approxSymbol (parseSymbol c <> parseSymbol d)

instance Monoid BoxChar where
  mempty = BoxChar ' '
  mappend = (<>)

instance Universe Thickness where
  universe = [Thin, Thick]

instance Finite Thickness

instance Universe LineStyle where
  universe = Double : ([Dashed, Solid] <*> universe)

instance Finite LineStyle

instance Universe l => Universe (Style l) where
  universe = Transparent : White : (Line <$> universe)

instance Finite l => Finite (Style l)

instance Universe a => Universe (Plus a) where
  universe = do
    !(u :: a) <- universe
    !(l :: a) <- universe
    !(r :: a) <- universe
    !(d :: a) <- universe
    return $ makePlus u l r d

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
    mconcat (onCardinals (getSegment (fmap f p)))

instance Traversable Plus where
  sequenceA (Plus f) =
    makePlus <$> f U <*> f L <*> f R <*> f D

instance Show a => Show (Plus a) where
  showsPrec d !(Plus f) =
    showParen (d > 10) . showString $
      "makePlus "
      ++ intercalate " "
           (map (($ []) . showsPrec 11) (onCardinals f))

deriving instance Show (Cardinal d)
deriving instance Ord  (Cardinal d)
deriving instance Eq   (Cardinal d)

associative :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associative op a b c = (a `op` b) `op` c == a `op` (b `op` c)
