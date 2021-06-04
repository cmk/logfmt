{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Internal format starters.
module Data.Fmt (

    -- * Types
    LogFmt,
    Fmt1,
    Fmt (..),

    -- * Fmt
    fmt,
    fmt1,
    logFmt,
    runFmt,
    format,
    printf,
    Term,
    
    -- * Transforms
    (%),
    apply,
    bind,
    const1,
    const2,
    remove,
    replace,
    reformat,
    catWith,
    --splitWith,
    
    -- * Formatting
    cat,
    spaces,
    indent,
    prefix,
    suffix,
    enclose,
    tuple,
    quotes,
    quotes',
    parens,
    angles,
    braces,
    brackets,
    backticks,

    -- * Formatters
    v,

    -- ** Char
    c,
    c7,
    c8,
    
    -- ** String
    s,
    sh,
    s7,
    s8,
    
    -- ** Float
    e,
    f,
    g,

    -- ** Signed int
    d,
    hhd,
    hd,
    ld,
    lld,

    -- ** Unsigned int
    u,
    hhu,
    hu,
    lu,
    llu,

    -- ** Hexadecimal
    x,
    hhx,
    hx,
    hx',
    lx,
    lx',
    llx,
    llx',

    -- ** Binary
    b,
    b',
    hhb,
    hb,
    hb',
    lb,
    lb',
    llb,
    llb',

    -- ** Collections
    --hsep,
    --vsep,
    --hang,
    list,
    jsonList,
    yamlList,
    jsonMap,
    yamlMap,
    maybe1,
    either1,
    
    -- * Re-exports
    LogStr,
    fromLogStr,
    ToLogStr(..)

) where

import Data.Fixed (Fixed, HasResolution, showFixed)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (transpose, intersperse)
import Data.Ratio (Ratio, denominator, numerator)
import Data.Word

import Control.Applicative (Const (..), getConst)
import Control.Arrow ((&&&))
import Control.Category (Category (), (<<<), (>>>))
import Data.Char (chr, ord)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Monoid
import Data.String
import qualified Data.List as L
import Data.Int
import Data.Word
import Data.Fmt.Type

--import Data.ByteString (ByteString)
--import qualified Data.Text as T
--import qualified Data.Text.Lazy as TL
--import qualified Data.Text.Lazy.Builder as TL
import qualified Control.Category as C
import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BL
import GHC.Exts (IsList, Item)
import qualified GHC.Exts as IsList (toList)
import System.Log.FastLogger (LogStr, fromLogStr, ToLogStr(..))
import qualified Numeric as N -- (showEFloat,showFFloat,showIntAtBase)
import System.IO

import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Sieve

--import Data.Printf

import System.Console.ANSI.Codes
import System.Console.ANSI.Types
import Data.Maybe
import Text.Show

-- $setup
-- >>> import Test.Contra.Fmt



type LogFmt = Fmt LogStr

logFmt :: ToLogStr m => Fmt m s a -> Fmt LogStr s a
logFmt = refmt toLogStr
--logFmt :: ToLogStr m => m -> Fmt LogStr a a
--logFmt = fmt . toLogStr

--format :: ToLogStr m => Fmt m LogStr a -> a
--format = flip unFmt toLogStr

--format :: Fmt LogStr B.ByteString a -> a
--format = flip unFmt fromLogStr

format :: IsString s => Fmt LogStr s a -> a
format = flip unFmt (fromString . B.unpack . fromLogStr)
{-# Specialize format :: Fmt LogStr BL.ByteString a -> a #-}
{-# Specialize format :: Fmt LogStr ByteString a -> a #-}
{-# Specialize format :: Fmt LogStr String a -> a #-}
{-# Specialize format :: Fmt LogStr LogStr a -> a #-}
{-# Specialize format :: Fmt LogStr Builder a -> a #-}

-- | Run the formatter and print out the text to stdout.
printf :: Fmt LogStr Term a -> a
printf = flip unFmt (B.putStr . fromLogStr)

type Term = IO ()

-- Transformation

-------------------------

-- | Filter the formatted string to not contain characters which pass the given predicate:
--
-- >>> format (remove Data.Char.isUpper t) "Data.Char.isUpper"
-- "ata.har.ispper"
remove :: (Char -> Bool) -> Fmt LogStr s a -> Fmt LogStr s a
remove p = reformat (B.filter (not . p))
{-# INLINE remove #-}

-- | Filter the formatted string to contain only characters which pass the given predicate:
--
-- >>> format (replace Data.Char.isUpper t) "Data.Char.isUpper"
-- "DCU"
replace :: Char -> (Char -> Bool) -> Fmt LogStr s a -> Fmt LogStr s a
replace c p = reformat (B.map $ \x -> if p x then c else x)
{-# INLINE replace #-}

-- | Alter the formatted string with the given function.
--
-- >>> format (reformat BL.reverse d) 123456
-- "654321"
reformat :: ToLogStr b => (ByteString -> b) -> Fmt LogStr s a -> Fmt LogStr s a
reformat f = refmt (toLogStr . f . fromLogStr)
{-# INLINABLE reformat #-}

-- | Use the given text-joining function to join together the individually rendered items of a list.
--
-- >>> format (catWith (mconcat . reverse) d) [123, 456, 789]
-- "789456123"
--
-- @
-- 'catWith' 'L.unlines' :: 'Foldable' f => 'Fmt1' 'LogStr' 'String' a -> 'Fmt1' 'LogStr' s (f a)
-- 'catWith' 'T.unlines' :: 'Foldable' f => 'Fmt1' 'LogStr' 'T.Text' a -> 'Fmt1' 'LogStr' s (f a)
-- 'catWith' 'B.unlines' :: 'Foldable' f => 'Fmt1' 'LogStr' 'B.ByteString' a -> 'Fmt1' 'LogStr' s (f a)
-- 'catWith' '$' 'L.intercalate' " " :: 'Foldable' f => 'Fmt1' 'LogStr' 'String' a -> 'Fmt1' 'LogStr' s (f a)
-- 'catWith' '$' 'T.intercalate' " " :: 'Foldable' f => 'Fmt1' 'LogStr' 'T.Text' a -> 'Fmt1' 'LogStr' s (f a)
-- 'catWith' '$' 'B.intercalate' " " :: 'Foldable' f => 'Fmt1' 'LogStr' 'B.ByteString' a -> 'Fmt1' 'LogStr' s (f a)
-- @
{-# INLINABLE catWith #-}
catWith ::
  (Foldable f, ToLogStr str, IsString str) =>
  ([str] -> str) ->
  Fmt1 LogStr str a ->
  Fmt1 LogStr s (f a)
catWith join f = fmt1 $ toLogStr . join . fmap (format f) . toList

{-# Specialize catWith :: Foldable f => ([LogStr] -> LogStr) -> Fmt1 LogStr LogStr a -> Fmt1 LogStr s (f a) #-}
{-# Specialize catWith :: Foldable f => ([Builder] -> Builder) -> Fmt1 LogStr Builder a -> Fmt1 LogStr s (f a) #-}
{-# Specialize catWith :: Foldable f => ([ByteString] -> ByteString) -> Fmt1 LogStr ByteString a -> Fmt1 LogStr s (f a) #-}
{-# Specialize catWith :: Foldable f => ([BL.ByteString] -> BL.ByteString) -> Fmt1 LogStr BL.ByteString a -> Fmt1 LogStr s (f a) #-}

-- | Utility for taking a text-splitting function and turning it into a formatting combinator.
--
-- @
--  'splitWith' 'cat'  :: ('Traversable' f, 'ToLogStr' msg) => ('ByteString' -> f msg) -> 'Fmt' 'LogStr' s a -> 'Fmt' 'LogStr' s a
--  'splitWith' 'hsep' :: ('Traversable' f, 'ToLogStr' msg) => ('ByteString' -> f msg) -> 'Fmt' 'LogStr' s a -> 'Fmt' 'LogStr' s a
--  'splitWith' 'vsep' :: ('Traversable' f, 'ToLogStr' msg) => ('ByteString' -> f msg) -> 'Fmt' 'LogStr' s a -> 'Fmt' 'LogStr' s a
--  'splitWith' 'list' :: ('Traversable' f, 'ToLogStr' msg) => ('ByteString' -> f msg) -> 'Fmt' 'LogStr' s a -> 'Fmt' 'LogStr' s a
-- @
-- >>> commas = reverse . fmap BL.reverse . BL.chunksOf 3 . BL.reverse
-- >>> dollars = prefix "$" . splitWith commas (intercalate ",") . reversed
-- >>> format (dollars d) 1234567890
-- "$1,234,567,890"
-- >>> printf (splitWith (BL.splitOn ",") vsep t) "one,two,three"
-- one
-- two
-- three
-- >>> printf (splitWith (BL.splitOn ",") (indentEach 4) t) "one,two,three"
--     one
--     two
--     three
{-# INLINABLE splitWith #-}
splitWith ::
  (Traversable f, ToLogStr str) =>
  (Fmt1 m s_ m -> Fmt1 m m (f LogStr)) ->
  (ByteString -> f str) ->
  Fmt LogStr s a ->
  Fmt m s a
splitWith lf split (Fmt g) = Fmt (g . (. runFmt (lf $ fmt1 id) . fmap toLogStr . split . fromLogStr)) 


-- Formatters

-------------------------

-- | Encode a loggable value.
--
-- Semantics are similar to 'ByteString.Printf.printf':
--
-- >>> Text.Printf.printf "%v" 42 :: String
-- "42"
-- >>> format v 42
-- "42"
{-# INLINE v #-}
v :: ToLogStr a => Fmt1 LogStr s a
v = fmt1 toLogStr

-- | ASCII encode a 'Char'.
{-# INLINE c7 #-}
c7 :: Fmt1 LogStr s Char
c7 = fmt1 $ toLogStr . BL.char7

-- | Latin-1 (ISO/IEC 8859-1) encode a 'Char'.
{-# INLINE c8 #-}
c8 :: Fmt1 LogStr s Char
c8 = fmt1 $ toLogStr . BL.char8

-- | ASCII encode a 'String'.
{-# INLINE s7 #-}
s7 :: Fmt1 LogStr s String
s7 = fmt1 $ toLogStr . BL.string7

-- | Latin-1 (ISO/IEC 8859-1) encode a 'String'.
{-# INLINE s8 #-}
s8 :: Fmt1 LogStr s String
s8 = fmt1 $ toLogStr . BL.string8


-- Signed integers

-------------------------

-- | Decimal encoding of an 'Int' using the ASCII digits.
{-# INLINE d #-}
d :: Fmt1 LogStr s Int
d = fmt1 $ toLogStr . BL.intDec

-- | Decimal encoding of an 'Int8' using the ASCII digits.
--
-- e.g.
--
-- > toLazyByteString (int8Dec 42)   = "42"
-- > toLazyByteString (int8Dec (-1)) = "-1"
--
{-# INLINE hhd #-}
hhd :: Fmt1 LogStr s Int8
hhd = fmt1 $ toLogStr . BL.int8Dec

-- | Decimal encoding of an 'Int16' using the ASCII digits.
{-# INLINE hd #-}
hd :: Fmt1 LogStr s Int16
hd = fmt1 $ toLogStr . BL.int16Dec

-- | Decimal encoding of an 'Int32' using the ASCII digits.
{-# INLINE ld #-}
ld :: Fmt1 LogStr s Int32
ld = fmt1 $ toLogStr . BL.int32Dec

-- | Decimal encoding of an 'Int64' using the ASCII digits.
{-# INLINE lld #-}
lld :: Fmt1 LogStr s Int64
lld = fmt1 $ toLogStr . BL.int64Dec

-- Unsigned integers

-------------------------

-- | Decimal encoding of a 'Word' using the ASCII digits.
{-# INLINE u #-}
u :: Fmt1 LogStr s Word
u = fmt1 $ toLogStr . BL.wordDec

-- | Decimal encoding of a 'Word8' using the ASCII digits.
{-# INLINE hhu #-}
hhu :: Fmt1 LogStr s Word8
hhu = fmt1 $ toLogStr . BL.word8Dec

-- | Decimal encoding of a 'Word16' using the ASCII digits.
{-# INLINE hu #-}
hu :: Fmt1 LogStr s Word16
hu = fmt1 $ toLogStr . BL.word16Dec

-- | Decimal encoding of a 'Word32' using the ASCII digits.
{-# INLINE lu #-}
lu :: Fmt1 LogStr s Word32
lu = fmt1 $ toLogStr . BL.word32Dec

-- | Decimal encoding of a 'Word64' using the ASCII digits.
{-# INLINE llu #-}
llu :: Fmt1 LogStr s Word64
llu = fmt1 $ toLogStr . BL.word64Dec

-- Hexadecimal
--------------------

-- | Shortest hexadecimal encoding of a 'Word' using lower-case characters.
{-# INLINE x #-}
x :: Fmt1 LogStr s Word
x = fmt1 $ toLogStr . BL.wordHex

-- | Shortest hexadecimal encoding of a 'Word8' using lower-case characters.
{-# INLINE hhx #-}
hhx :: Fmt1 LogStr s Word8
hhx = fmt1 $ toLogStr . BL.word8Hex

-- | Encode a 'Word8' using 2 nibbles (hexadecimal digits).
{-# INLINE hhx' #-}
hhx' :: Fmt1 LogStr s Word8
hhx' = fmt1 $ toLogStr . BL.word8HexFixed

-- | Shortest hexadecimal encoding of a 'Word16' using lower-case characters.
{-# INLINE hx #-}
hx :: Fmt1 LogStr s Word16
hx = fmt1 $ toLogStr . BL.word16Hex

-- | Encode a 'Word16' using 4 nibbles.
{-# INLINE hx' #-}
hx' :: Fmt1 LogStr s Word16
hx' = fmt1 $ toLogStr . BL.word16HexFixed

-- | Shortest hexadecimal encoding of a 'Word32' using lower-case characters.
{-# INLINE lx #-}
lx :: Fmt1 LogStr s Word32
lx = fmt1 $ toLogStr . BL.word32Hex

-- | Encode a 'Word32' using 8 nibbles.
{-# INLINE lx' #-}
lx' :: Fmt1 LogStr s Word32
lx' = fmt1 $ toLogStr . BL.word32HexFixed

-- | Shortest hexadecimal encoding of a 'Word64' using lower-case characters.
--
-- Semantics are similar to 'Text.Printf.printf':
--
-- >>> P.printf "%s: %llx" "Val" (-7) :: String
-- "Val: fffffffffffffff9"
-- >>> printf (s % ": " % llx) "Val" (-7)
-- "Val: fffffffffffffff9"
{-# INLINE llx #-}
llx :: Fmt1 LogStr s Word64
llx = fmt1 $ toLogStr . BL.word64Hex

-- | Encode a 'Word64' using 16 nibbles.
{-# INLINE llx' #-}
llx' :: Fmt1 LogStr s Word64
llx' = fmt1 $ toLogStr . BL.word64HexFixed

-- Binary 
--------------------

-- | Format a lazy byte string.
b :: Fmt1 LogStr s BL.ByteString
b = fmt1 toLogStr
{-# INLINE b #-}

-- | Format a strict byte string.
--
-- @ 'fmap' (. 'Data.ByteString.pack') 't'' :: 'Fmt1' 'Data.ByteString.Builder.Builder' s 'String' @
b' :: Fmt1 LogStr s ByteString
b' = fmt1 toLogStr
{-# INLINE b' #-}

-- | Encode a 'Word8' as-is
{-# INLINE hhb #-}
hhb :: Fmt1 LogStr s Word8
hhb = fmt1 $ toLogStr . BL.word8

-- | Encode a 'Word16' using little-endian format.
{-# INLINE hb #-}
hb :: Fmt1 LogStr s Word16
hb = fmt1 $ toLogStr . BL.word16LE

-- | Encode a 'Word16' using big-endian format.
{-# INLINE hb' #-}
hb' :: Fmt1 LogStr s Word16
hb' = fmt1 $ toLogStr . BL.word16BE

-- | Encode a 'Word32' using little-endian format.
{-# INLINE lb #-}
lb :: Fmt1 LogStr s Word32
lb = fmt1 $ toLogStr . BL.word32LE

-- | Encode a 'Word32' using big-endian format.
{-# INLINE lb' #-}
lb' :: Fmt1 LogStr s Word32
lb' = fmt1 $ toLogStr . BL.word32BE

-- | Encode a 'Word64' using little-endian format.
--
-- Semantics are similar to 'Tebt.Printf.printf':
--
-- >>> P.printf "%s: %llb" "Val" (-7) :: String
-- "Val: fffffffffffffff9"
-- >>> printf (s % ": " % llb) "Val" (-7)
-- "Val: fffffffffffffff9"
{-# INLINE llb #-}
llb :: Fmt1 LogStr s Word64
llb = fmt1 $ toLogStr . BL.word64LE

-- | Encode a 'Word64' using big-endian format.
--
{-# INLINE llb' #-}
llb' :: Fmt1 LogStr s Word64
llb' = fmt1 $ toLogStr . BL.word64BE



-- Collections

-------------------------


-- | Format each value in a list with spaces in between:
--
-- >>> format (hsep d) [1, 2, 3]
-- "1 2 3"
hsep :: Foldable f => Fmt1 LogStr ByteString a -> Fmt1 LogStr s (f a)
hsep = catWith $ B.intercalate " "
{-# INLINE hsep #-}

-- | Format each value in a list, placing each on its own line:
--
-- >>> printf (vsep c) ['a'..'c']
-- a
-- b
-- c
vsep :: Foldable f => Fmt1 LogStr ByteString a -> Fmt1 LogStr s (f a)
vsep = catWith B.unlines
{-# INLINE vsep #-}

-- | Format a list of items, placing one per line, indent by the given number of spaces.
--
-- @ 'indentEach' n = 'Test.Contra.Type.Format.vsep' . 'indent' n @
--
-- >>> printf (splitWith BL.lines (indentList 2) t) "one\ntwo\nthree"
--   one
--   two
--   three
-- >>> printf ("The lucky numbers are:\n" % indentList 2 d) [7, 13, 1, 42]
-- The lucky numbers are:
--   7
--   13
--   1
--   42
hang :: Foldable f => Int -> Fmt1 LogStr ByteString a -> Fmt1 LogStr s (f a)
hang n = vsep . indent n
{-# INLINE hang #-}


--  | Attach a name to a formatter.
--
-- >>> printf (name "clients" $ yamlList s) ["Alice", "Bob", "Zalgo"]
-- clients:
--   - Alice
--   - Bob
--   - Zalgo
--name :: LogStr -> Fmt LogStr s a -> Fmt LogStr s a
--name b = refmt (nameF b)

-- | Add square brackets around the Foldable (e.g. a list), and separate each formatted item with a comma and space.
--
-- >>> format (list s) ["one", "two", "three"]
-- "[one, two, three]"
-- >>> printf (quotes $ list d) [1,2,3]
-- ["1", "2", "3"]
-- >>> printf (quotes $ list s) ["one", "two", "three"]
-- ["one", "two", "three"]
list :: Foldable f => Fmt1 LogStr ByteString a -> Fmt1 LogStr s (f a)
list = catWith (B.intercalate ", ") . brackets
{-# INLINE list #-}

-- | A JSON-style formatter for lists.
--
-- >>> fmt $ jsonListF [1,2,3]
-- [
--   1
-- , 2
-- , 3
-- ]
--
-- Like 'yamlListF', it handles multiline elements well:
--
-- >>> fmt $ jsonListF ["hello\nworld", "foo\nbar\nquix"]
-- [
--   hello
--   world
-- , foo
--   bar
--   quix
-- ]
{-# INLINE jsonList #-}
jsonList :: (Foldable f, ToLogStr a) => Fmt1 LogStr s (f a)
jsonList = fmt1 f
  where
    f xs
        | null items = "[]\n"
        | otherwise = "[\n" <> mconcat items <> "]\n"
      where
        items = zipWith buildItem (True : repeat False) (toList xs)
        -- Item builder
        --buildItem :: Bool -> a -> B
        
        buildItem isFirst x =
            case map toLogStr (B.lines (fromLogStr (toLogStr x))) of
                []
                    | isFirst -> "\n"
                    | otherwise -> ",\n"
                (h : t) ->
                    mconcat . map (<> "\n") $
                        if isFirst
                            then "  " <> h : fmap ("  " <>) t
                            else ", " <> h : fmap ("  " <>) t


-- | A multiline formatter for lists.
--
--  >>> printf (yamlList d) [1,2,3]
--  - 1
--  - 2
--  - 3
--
--  Multi-line elements are indented correctly:
--
--  >>> printf (yamlList s) ["hello\nworld", "foo\nbar\nquix"]
--  - hello
--    world
--  - foo
--    bar
--    quix
{-# INLINE yamlList #-}
yamlList :: (Foldable f, ToLogStr a) => Fmt1 LogStr s (f a)
yamlList = fmt1 f
  where
    f xs = if null items then "[]\n" else mconcat items
      where
        bullet = "-"
        spaces = "  "
        newline = "\n"
        items = map buildItem (toList xs)
        buildItem x = case B.lines (fromLogStr (toLogStr x)) of
          []     -> bullet <> newline
          (l:ls) -> bullet <> " " <> toLogStr l <> newline <>
                    mconcat [spaces <> toLogStr s <> newline | s <- ls]

{- | A JSON-like map formatter; works for Map, HashMap, etc, and lists of pairs.

>>> fmt $ jsonMapF [("Odds", jsonListF [1,3]), ("Evens", jsonListF [2,4])]
{
  Odds:
    [
      1
    , 3
    ]
, Evens:
    [
      2
    , 4
    ]
}
-}
{-# INLINE jsonMap #-}
jsonMap :: (ToLogStr k, IsList map, Item map ~ (k, ByteString)) => Fmt1 LogStr s map
jsonMap = fmt1 f
  where
    f xs
      | null items = "{}\n"
      | otherwise  = "{\n" <> mconcat items <> "}\n"
      where
        items = zipWith buildItem (True : repeat False) (IsList.toList xs)
        -- Item builder
        --buildItem :: Bool -> (k, v) -> B
        buildItem isFirst (k, v) = do
          let kb = (if isFirst then "  " else ", ") <> toLogStr k
          case map toLogStr (B.lines v) of
            []  -> kb <> ":\n"
            [l] -> kb <> ": " <> l <> "\n"
            ls  -> kb <> ":\n" <>
                   mconcat ["    " <> s <> "\n" | s <- ls]

--  | A YAML-like map formatter:
--
-- >>> BL.putStrLn $ BL.toLazyByteString $ yamlMapF id id [("Odds", yamlListF (BL.fromString . show) "-" [1,3]), ("Evens", yamlListF (BL.fromString . show) "-" [2,4])]
-- Odds:
--   - 1
--   - 3
-- Evens:
--   - 2
--   - 4
{-# INLINE yamlMap #-}
yamlMap :: (ToLogStr k, ToLogStr v, IsList map, Item map ~ (k, v)) => Fmt1 LogStr s map
yamlMap = fmt1 f
  where
    f xs | null items = "{}\n"
         | otherwise = mconcat items
      where
        items = map (\(k, v) -> nameF (toLogStr k) (toLogStr v)) (IsList.toList xs)

nameF :: LogStr -> LogStr -> LogStr
nameF k v = case B.lines (fromLogStr v) of
    [] -> k <> ":\n"
    [l] -> k <> ": " <> toLogStr l <> "\n"
    ls ->
        k <> ":\n"
            <> mconcat ["  " <> toLogStr s <> "\n" | s <- ls]








-- Internal

-------------------------







{-

-- ByteString formatters

--  |
-- Indent a block of text.
--
-- >>> fmt $ "This is a list:\n" <> indentF 4 (yamlListF [1,2,3])
-- This is a list:
--     - 1
--     - 2
--     - 3
--
-- The output will always end with a newline, even when the input doesn't.
--indents :: Int -> B -> B
indents i = reformat (f i)
  where
    f n a = case BL.lines a of
              [] -> spaces n <> "\n"
              xs -> BL.unlines (map (spaces n <>) xs)
    spaces n = BL.replicate (fromIntegral n) (BL.singleton ' ')


--  | Add a prefix to the first line, and indent all lines but the first one.
--
-- The output will always end with a newline, even when the input doesn't.
indentF' :: Int -> B.ByteString -> B -> B
indentF' n pref a = case BL.lines (BL.toLazyByteString a) of
    [] -> BL.byteString pref <> "\n"
    (x : xs) ->
        BL.lazyByteString $
            BL.vsep $ (BL.fromStrict pref <> x) : map (spaces <>) xs
  where
    spaces = BL.replicate (fromIntegral n) (BL.singleton ' ')

--  |
-- Take the first N characters:
--
-- >>> prefixF 3 "hello"
-- "hel"
prefixF :: Buildable a => Int -> a -> B
prefixF size =
    BL.lazyByteString . BL.take (fromIntegral size) . BL.toLazyByteString . build

--  |
-- Take the last N characters:
--
-- >>> suffixF 3 "hello"
-- "llo"
suffixF :: Buildable a => Int -> a -> B
suffixF size =
    BL.lazyByteString
        . (\t -> BL.drop (BL.length t - fromIntegral size) t)
        . BL.toLazyByteString
        . build

-- --------------------------------------------------------------------------
--  List formatters
-- --------------------------------------------------------------------------

--  | A simple comma-separated list formatter.
--
-- >>> listF ["hello", "world"]
-- "[hello, world]"
--
-- For multiline output, use 'jsonListF'.
{-# INLINE listF #-}
listF :: Foldable f => (a -> B) -> f a -> B
listF build xs =
    mconcat $
        "[" :
        intersperse ", " (map build (toList xs))
            ++ ["]"]

-- | Put spaces between elements.
--
--  >>> fmt $ hsepF ["hello", "world"]
--  hello world
--
--  Of course, it works on anything 'Buildable':
--
--  >>> fmt $ hsepF [1, 2]
--  1 2
hsepF :: Foldable f => (a -> B) -> f a -> B
hsepF build = mconcat . intersperse " " . map build . toList
{-# SPECIALIZE hsepF :: Buildable a => [a] -> B #-}

-- | Arrange elements on separate lines.
--
--  >>> fmt $ vsepF ["hello", "world"]
--  hello
--  world
vsepF :: Foldable f => (a -> B) -> f a -> B
vsepF build = mconcat . map (nl . build) . toList
  where
    nl x
        | "\n" `BL.isSuffixOf` BL.toLazyByteString x = x
        | otherwise = x <> "\n"
{-# SPECIALIZE vsepF :: Buildable a => [a] -> B #-}


-}

