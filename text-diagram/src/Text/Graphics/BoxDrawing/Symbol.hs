module Text.Graphics.BoxDrawing.Symbol
  ( Symbol
  , getSymbol
  , makeSymbol
  , exactSymbol
  , approxSymbol
  , parseSymbol
  ) where

import Data.Semigroup
import Data.String
import Data.Universe
import Data.Bits
import Data.Maybe
import Data.List
import Data.Foldable
import Data.Traversable

import Text.Graphics.BoxDrawing.Style
import Text.Graphics.BoxDrawing.Cardinal
import Text.Graphics.BoxDrawing.Plus


-- -----------------
-- Syntactic symbols
-- -----------------

-- Box drawing characters, restricted to those represented by our framework
-- We don't use the entire Unicode block, and you can't construct unused ones

newtype Symbol
  = Symbol Char
  deriving (Eq, Ord, Show)

getSymbol :: Symbol -> Char
getSymbol (Symbol c) = c

boxCharNum :: Symbol -> Int
boxCharNum (Symbol c)
  | c == ' '  = 128
  | otherwise = fromEnum c - 0x2500

unsafeSymbol :: Int -> Symbol
unsafeSymbol i
  | i == 128 = Symbol ' '
  | i >= 0 && i < 128 = Symbol (toEnum (i + 0x2500))
  | otherwise = error "unsafeSymbol: out of range"

instance Semigroup Symbol where
  c <> d = approxSymbol (parseSymbol c <> parseSymbol d)

instance Monoid Symbol where
  mempty = Symbol ' '
  mappend = (<>)

instance Universe Symbol where
  universe = listSS allSymbols

instance Finite Symbol


-- -------------------------------
-- Sets of symbols (internal only)
-- -------------------------------

-- Sets of Symbols, efficiently packed as bit-vectors
-- There are potentially 129 (yes, I know) characters:
-- 128 from the unicode block + the space character ' '

data SymbolSet
  = SS !Word !Word !Bool
  deriving (Eq, Ord)

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
parseSymbol s =
  fromMaybe
    (error $ "parseSymbol: not found (should be impossible)" ++ show s) $
    for symbols $ \f ->
      find (memberSS s . f) universe
