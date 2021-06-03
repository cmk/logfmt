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

    Term,
    LogFmt,

    -- * Fmt
    Fmt1,
    Fmt2,
    Fmt3,
    Fmt (..),

    printf,
    format,
    runFmt,
    runLogFmt,
    
    -- ** Introduction
    fmt,
    fmts,
    fmt1,
    fmt2,
    
    -- ** Transformation
    const1,
    const2,
    refmt,
    apply,
    bind,
    (%),
    
    remove,
    replace,
    reformat,

    -- * Formatters

    -- ** Char
    c,
    c7,
    c8,
    
    -- ** String
    s,
    s7,
    s8,
    sh,
    v,
    
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
    cat,
    left1,
    right1,
    either1,
    maybe1,

    -- * Formatting
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

    -- ** Collections
    hsep,
    vsep,
    hang,
    catWith,
    intercalate,
    list,
    jsonList,
    yamlList,
    jsonMap,
    yamlMap,
    
    -- ** Indentation & Splitting
    name,
    splitWith,

    -- * Ansi terminal codes
    code,
    codes,
    erase,
    reset,
    shift,
    scroll,
    
    -- ** Emphasis
    blink,
    bold,
    faint,
    italic,
    underline,

    -- ** Color
    dull,
    vivid,
    layer,
    palette,
    Palette,
    XColor,
    Color (..),
    ConsoleLayer (..),

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

--import Data.ByteString (ByteString)
--import qualified Data.Text as T
--import qualified Data.Text.Lazy as TL
--import qualified Data.Text.Lazy.Builder as TL
import qualified Control.Category as C
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (ByteString)
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


type Term = IO ()

type LogFmt = Fmt Builder

-- | Format a value of type @a@.
--
-- @ 'Fmt1' m s a ~ (m -> s) -> a -> s @
type Fmt1 m s a = Fmt m s (a -> s)

-- | Format two values of type @a@ and @b@.
--
-- @ 'Fmt2' m s a b ~ (m -> s) -> a -> b -> s @
type Fmt2 m s a b = Fmt m s (a -> b -> s)

-- | Format three values of type @a@, @b@ and @c@.
--
-- @ 'Fmt3' m s a b c ~ (m -> s) -> a -> b -> c -> s @
type Fmt3 m s a b c = Fmt m s (a -> b -> c -> s)


-- | A formatter, implemented as an indexed continuation
--
-- When you construct formatters the first type
-- parameter, @r@, will remain polymorphic.  The second type
-- parameter, @a@, will change to reflect the types of the data that
-- will be formatted.  For example, in
--
-- @
-- person :: Fmt2 ByteString Int
-- person = \"Person's name is \" % t % \", age is \" % d
-- @
--
-- the first type parameter remains polymorphic, and the second type
-- parameter is @ByteString -> Int -> r@, which indicates that it formats a
-- 'ByteString' and an 'Int'.
--
-- When you run the formatter, for example with 'format', you provide
-- the arguments and they will be formatted into a string.
--
-- >>> format ("This person's name is " % s % ", their age is " % d) "Anne" 22
-- "This person's name is Anne, their age is 22"
newtype Fmt m s a = Fmt {unFmt :: (m -> s) -> a}


-- | Apply two formatters to the same input argument.
--
instance Semigroup m => Semigroup (Fmt1 m a b) where
    m <> n =
        Fmt
            ( \k a ->
                unFmt m (\b1 -> unFmt n (\b2 -> k (b1 <> b2)) a) a
            )

instance Monoid m => Monoid (Fmt1 m a b) where
    mempty = Fmt (\k _ -> k mempty)

instance (IsString m, a ~ b) => IsString (Fmt m a b) where
    fromString = fmt . fromString

instance Monoid m => Category (Fmt m) where
    id = fmt mempty
    (.) = (%)

deriving via (Costar ((->) m) a) instance Functor (Fmt m a)
deriving via (Costar ((->) m) a) instance Applicative (Fmt m a)
deriving via (Costar ((->) m) a) instance Monad (Fmt m a)
deriving via (Costar ((->) m)) instance Profunctor (Fmt m)
deriving via (Costar ((->) m)) instance Closed (Fmt m)
deriving via (Costar ((->) m)) instance Costrong (Fmt m)
deriving via (Costar ((->) m)) instance Cochoice (Fmt m)

instance Cosieve (Fmt m) ((->) m) where
    cosieve (Fmt f) = f

instance Corepresentable (Fmt m) where
    type Corep (Fmt m) = ((->) m) 
    cotabulate f = (Fmt f)

-- | Run the formatter and print out the text to stdout.
printf :: Fmt Builder Term a -> a
printf = flip unFmt (BL.putStr . BL.toLazyByteString)

format :: IsString s => Fmt Builder s a -> a
format = flip unFmt (fromString . BL.unpack . BL.toLazyByteString)
{-# Specialize format :: Fmt Builder ByteString a -> a #-}
{-# Specialize format :: Fmt Builder B.ByteString a -> a #-}
{-# Specialize format :: Fmt Builder String a -> a #-}
{-# Specialize format :: Fmt Builder Builder a -> a #-}

runFmt :: Fmt m m a -> a
runFmt = flip unFmt id

runLogFmt :: ToLogStr m => Fmt m LogStr a -> a
runLogFmt = flip unFmt toLogStr



--logFmt :: ToLogStr m => m -> Fmt LogStr a a
--logFmt = fmt . toLogStr
--toLogFmt :: ToLogStr m => Fmt m s a -> Fmt LogStr s a
--toLogFmt = refmt toLogStr
--runLogFmt :: Fmt LogStr B.ByteString a -> a
--runLogFmt = flip unFmt fromLogStr
-- | Run the formatter and print out the text to stdout.
--printf :: Fmt LogStr Term a -> a
--printf = flip unFmt (B.putStr . fromLogStr)

-- Introduction

-------------------------

-- | Format a constant value of type @m@.
--
fmt :: m -> Fmt m a a
fmt b = Fmt ($ b)

fmts :: Monoid m => (s -> a) -> Fmt m s a
fmts f = Fmt $ \k -> f (k mempty)

-- | Format a value of type @a@ using a function of type @a -> m@.
--
-- @ 'runFmt' . 'fmt1' :: (a -> m) -> a -> m @
--
fmt1 :: (a -> m) -> Fmt1 m s a
fmt1 f = Fmt $ \k -> k . f

fmt2 :: (a -> b -> m) -> Fmt2 m s a b
fmt2 f = Fmt $ \k -> fmap k . f

-- Transformation

-------------------------

const1 :: Fmt m s s -> Fmt1 m s a
const1 = lmap const . closed

const2 :: Fmt m s s -> Fmt2 m s a b
const2 = lmap (const.const) . (closed.closed)

-- | Map over the the formatting @Monoid@.
--
refmt :: (m1 -> m2) -> Fmt m1 s a -> Fmt m2 s a
refmt m12 (Fmt f) = Fmt $ \a -> f (a . m12)

apply :: Fmt1 m s m -> Fmt m s a -> Fmt m s a
apply (Fmt f) (Fmt a) = Fmt (a . f)

-- | Indexed bind.
--
bind :: Fmt m s1 a -> (m -> Fmt m s2 s1) -> Fmt m s2 a
bind m f = Fmt $ \k -> unFmt m (\a -> unFmt (f a) k)

-- | Concatenate two formatters.
--
infixr 9 %
(%) :: Semigroup m => Fmt m b c -> Fmt m a b -> Fmt m a c
f % g =
        f
          `bind` \a ->
              g
                  `bind` \b -> fmt (a <> b)


-- | Filter the formatted string to not contain characters which pass the given predicate:
--
-- >>> format (remove Data.Char.isUpper t) "Data.Char.isUpper"
-- "ata.har.ispper"
remove :: (Char -> Bool) -> Fmt B s a -> Fmt B s a
remove p = reformat (BL.filter (not . p))
{-# INLINE remove #-}

-- | Filter the formatted string to contain only characters which pass the given predicate:
--
-- >>> format (replace Data.Char.isUpper t) "Data.Char.isUpper"
-- "DCU"
replace :: Char -> (Char -> Bool) -> Fmt B s a -> Fmt B s a
replace c p = reformat (BL.map $ \x -> if p x then c else x)
{-# INLINE replace #-}

-- | Alter the formatted string with the given function.
--
-- >>> format (reformat BL.reverse d) 123456
-- "654321"
reformat :: (ByteString -> ByteString) -> Fmt Builder s a -> Fmt Builder s a
reformat f = refmt (BL.lazyByteString . f . BL.toLazyByteString)
{-# INLINABLE reformat #-}



-- Formatters

-------------------------

type B = Builder


-- Char

-------------------------

-- | Format a character.
c :: IsString m => Fmt1 m s Char
c = fmap (. pure) s
{-# INLINE c #-}

-- | ASCII encode a 'Char'.
{-# INLINE c7 #-}
c7 :: Fmt1 B s Char
c7 = fmt1 BL.char7

-- | Latin-1 (ISO/IEC 8859-1) encode a 'Char'.
{-# INLINE c8 #-}
c8 :: Fmt1 B s Char
c8 = fmt1 BL.char8

-- String

-------------------------

-- | Format a string.
s :: IsString m => Fmt1 m s String
s = fmt1 fromString
{-# INLINE s #-}

-- | ASCII encode a 'String'.
{-# INLINE s7 #-}
s7 :: Fmt1 B s String
s7 = fmt1 BL.string7

-- | Latin-1 (ISO/IEC 8859-1) encode a 'String'.
{-# INLINE s8 #-}
s8 :: Fmt1 B s String
s8 = fmt1 BL.string8

-- | Format a showable value.
--
-- Semantics are similar to 'ByteString.Printf.printf':
--
-- >>> ByteString.Printf.printf "%v" 42 :: String
-- "42"
-- >>> format v 42
-- "42"
{-# INLINE sh #-}
sh :: (IsString m, Show a) => Fmt1 m s a
sh = fmt1 (fromString . show)

{-# INLINE v #-}
v :: ToLogStr a => Fmt1 LogStr s a
v = fmt1 toLogStr

-- Floating point

-------------------------

-- | Format a floating point number to a given number of digits of precision.
--
-- Semantics are similar to 'ByteString.Printf.printf':
--
-- >>> ByteString.Printf.printf "%.5e" pi :: String
-- "3.14159e0"
-- >>> format (e 5) pi
-- "3.14159e0"
e :: (IsString m, RealFloat a) => Int -> Fmt1 m s a
e prec = fmt1 $ fromString . flip (N.showEFloat $ Just prec) []

-- | Format a floating point number to a given number of digits of precision.
--
-- Semantics are similar to 'ByteString.Printf.printf':
--
-- >>> ByteString.Printf.printf "%.5f" maximal32 :: String
-- "340282330000000000000000000000000000000.00000"
-- >>> format (f 5) maximal32
-- "340282330000000000000000000000000000000.00000"
f :: (IsString m, RealFloat a) => Int -> Fmt1 m s a
f prec = fmt1 $ fromString . flip (N.showFFloat $ Just prec) []

-- | Format a floating point number to a given number of digits of precision.
--
-- Semantics are similar to 'ByteString.Printf.printf':
--
-- >>> ByteString.Printf.printf "%.5g" maximal32 :: String
-- "3.40282e38"
-- >>> format (g 5) maximal32
-- "3.40282e38"
g :: (IsString m, RealFloat a) => Int -> Fmt1 m s a
g prec = fmt1 $ fromString . flip (N.showGFloat $ Just prec) []

-- Signed integers

-------------------------

-- | Decimal encoding of an 'Int' using the ASCII digits.
{-# INLINE d #-}
d :: Fmt1 B s Int
d = fmt1 BL.intDec

-- | Decimal encoding of an 'Int8' using the ASCII digits.
--
-- e.g.
--
-- > toLazyByteString (int8Dec 42)   = "42"
-- > toLazyByteString (int8Dec (-1)) = "-1"
--
{-# INLINE hhd #-}
hhd :: Fmt1 B s Int8
hhd = fmt1 BL.int8Dec

-- | Decimal encoding of an 'Int16' using the ASCII digits.
{-# INLINE hd #-}
hd :: Fmt1 B s Int16
hd = fmt1 BL.int16Dec

-- | Decimal encoding of an 'Int32' using the ASCII digits.
{-# INLINE ld #-}
ld :: Fmt1 B s Int32
ld = fmt1 BL.int32Dec

-- | Decimal encoding of an 'Int64' using the ASCII digits.
{-# INLINE lld #-}
lld :: Fmt1 B s Int64
lld = fmt1 BL.int64Dec

-- Unsigned integers

-------------------------

-- | Decimal encoding of a 'Word' using the ASCII digits.
{-# INLINE u #-}
u :: Fmt1 B s Word
u = fmt1 BL.wordDec

-- | Decimal encoding of a 'Word8' using the ASCII digits.
{-# INLINE hhu #-}
hhu :: Fmt1 B s Word8
hhu = fmt1 BL.word8Dec

-- | Decimal encoding of a 'Word16' using the ASCII digits.
{-# INLINE hu #-}
hu :: Fmt1 B s Word16
hu = fmt1 BL.word16Dec

-- | Decimal encoding of a 'Word32' using the ASCII digits.
{-# INLINE lu #-}
lu :: Fmt1 B s Word32
lu = fmt1 BL.word32Dec

-- | Decimal encoding of a 'Word64' using the ASCII digits.
{-# INLINE llu #-}
llu :: Fmt1 B s Word64
llu = fmt1 BL.word64Dec

-- Hexadecimal
--------------------

-- | Shortest hexadecimal encoding of a 'Word' using lower-case characters.
{-# INLINE x #-}
x :: Fmt1 B s Word
x = fmt1 BL.wordHex

-- | Shortest hexadecimal encoding of a 'Word8' using lower-case characters.
{-# INLINE hhx #-}
hhx :: Fmt1 B s Word8
hhx = fmt1 BL.word8Hex

-- | Encode a 'Word8' using 2 nibbles (hexadecimal digits).
{-# INLINE hhx' #-}
hhx' :: Fmt1 B s Word8
hhx' = fmt1 BL.word8HexFixed

-- | Shortest hexadecimal encoding of a 'Word16' using lower-case characters.
{-# INLINE hx #-}
hx :: Fmt1 B s Word16
hx = fmt1 BL.word16Hex

-- | Encode a 'Word16' using 4 nibbles.
{-# INLINE hx' #-}
hx' :: Fmt1 B s Word16
hx' = fmt1 BL.word16HexFixed

-- | Shortest hexadecimal encoding of a 'Word32' using lower-case characters.
{-# INLINE lx #-}
lx :: Fmt1 B s Word32
lx = fmt1 BL.word32Hex

-- | Encode a 'Word32' using 8 nibbles.
{-# INLINE lx' #-}
lx' :: Fmt1 B s Word32
lx' = fmt1 BL.word32HexFixed

-- | Shortest hexadecimal encoding of a 'Word64' using lower-case characters.
--
-- Semantics are similar to 'Text.Printf.printf':
--
-- >>> P.printf "%s: %llx" "Val" (-7) :: String
-- "Val: fffffffffffffff9"
-- >>> printf (s % ": " % llx) "Val" (-7)
-- "Val: fffffffffffffff9"
{-# INLINE llx #-}
llx :: Fmt1 B s Word64
llx = fmt1 BL.word64Hex

-- | Encode a 'Word64' using 16 nibbles.
{-# INLINE llx' #-}
llx' :: Fmt1 B s Word64
llx' = fmt1 BL.word64HexFixed

-- Binary 
--------------------

-- | Format a lazy byte string.
b :: Fmt1 B s ByteString
b = fmt1 BL.lazyByteString
{-# INLINE b #-}

-- | Format a strict byte string.
--
-- @ 'fmap' (. 'Data.ByteString.pack') 't'' :: 'Fmt1' 'Data.ByteString.Builder.Builder' s 'String' @
b' :: Fmt1 B s B.ByteString
b' = fmt1 BL.byteString
{-# INLINE b' #-}

-- | Encode a 'Word8' as-is
{-# INLINE hhb #-}
hhb :: Fmt1 B s Word8
hhb = fmt1 BL.word8

-- | Encode a 'Word16' using little-endian format.
{-# INLINE hb #-}
hb :: Fmt1 B s Word16
hb = fmt1 BL.word16LE

-- | Encode a 'Word16' using big-endian format.
{-# INLINE hb' #-}
hb' :: Fmt1 B s Word16
hb' = fmt1 BL.word16BE

-- | Encode a 'Word32' using little-endian format.
{-# INLINE lb #-}
lb :: Fmt1 B s Word32
lb = fmt1 BL.word32LE

-- | Encode a 'Word32' using big-endian format.
{-# INLINE lb' #-}
lb' :: Fmt1 B s Word32
lb' = fmt1 BL.word32BE

-- | Encode a 'Word64' using little-endian format.
--
-- Semantics are similar to 'Tebt.Printf.printf':
--
-- >>> P.printf "%s: %llb" "Val" (-7) :: String
-- "Val: fffffffffffffff9"
-- >>> printf (s % ": " % llb) "Val" (-7)
-- "Val: fffffffffffffff9"
{-# INLINE llb #-}
llb :: Fmt1 B s Word64
llb = fmt1 BL.word64LE

-- | Encode a 'Word64' using big-endian format.
--
{-# INLINE llb' #-}
llb' :: Fmt1 B s Word64
llb' = fmt1 BL.word64BE


-- Collections

-------------------------

-- | Format each value in a list and concatenate them all:
--
-- >>> runFmt (cat (s % " ")) ["one", "two", "three"]
-- "one two three "
--
cat :: (Monoid m, Foldable f) => Fmt1 m m a -> Fmt1 m s (f a)
cat f = fmt1 $ foldMap (runFmt f)
{-# INLINE cat #-}

-- | Render the value in a Left with the given formatter, rendering a Right as an empty string:
--
-- >>> format (left1 text) (Left "bingo")
-- "bingo"
--
-- >>> format (left1 text) (Right 16)
-- ""
left1 :: IsString m => Fmt1 m m a -> Fmt1 m s (Either a x)
left1 f = either1 f (fmt1 $ const "")
{-# INLINE left1 #-}

-- | Render the value in a Right with the given formatter, rendering a Left as an empty string:
--
-- >>> format (right1 text) (Left 16)
-- ""
--
-- >>> format (right1 text) (Right "bingo")
-- "bingo"
right1 :: IsString m => Fmt1 m m a -> Fmt1 m s (Either x a)
right1 = either1 (fmt1 $ const "")
{-# INLINE right1 #-}

-- | Render the value in an Either:
--
-- >>> format (either1 text int) (Left "Error!"
-- "Error!"
--
-- >>> format (either1 text int) (Right 69)
-- "69"
either1 :: Fmt1 m m a -> Fmt1 m m b -> Fmt1 m s (Either a b)
either1 l r = fmt1 $ either (runFmt l) (runFmt r)
{-# INLINE either1 #-}

-- | Render a Maybe value either as a default (if Nothing) or using the given formatter:
--
-- >>> format (maybe1 "Goodbye" text) Nothing
-- "Goodbye"
--
-- >>> format (maybe1 "Goodbye" text) (Just "Hello")
-- "Hello"
maybe1 :: m -> Fmt1 m m a -> Fmt1 m s (Maybe a)
maybe1 def f = fmt1 $ maybe def (runFmt f)
{-# INLINE maybe1 #-}



-- Formatting

-------------------------

-- | A string consisting of /n/ spaces.
spaces :: IsString m => Int64 -> m
spaces (fromIntegral -> n) = fromString $ replicate n ' '

-- | Insert the given number of spaces at the start of the rendered text:
--
-- >>> runFmt (indent 4 d) 7
-- "    7"
--
-- Note that this only indents the first line of a multi-line string.
-- To indent all lines see 'reindent'.
indent :: (IsString m, Semigroup m) => Int64 -> Fmt m s a -> Fmt m s a
indent = prefix . spaces
{-# INLINABLE indent #-}

-- | Add the given prefix to the formatted item:
--
-- >>> format ("The answer is: " % prefix "wait for it... " d) 42
-- "The answer is: wait for it... 42"
--
-- >>> printf (vsep (indent 4 (prefix "- " d))) [1, 2, 3]
--     - 1
--     - 2
--     - 3
prefix :: Semigroup m => m -> Fmt m s a -> Fmt m s a
prefix s f = fmt s % f
{-# INLINE prefix #-}

-- | Add the given suffix to the formatted item.
suffix :: Semigroup m => m -> Fmt m s a -> Fmt m s a
suffix s f = f % fmt s
{-# INLINE suffix #-}

-- | Enclose the output string with the given strings:
--
-- >>> runFmt (parens $ enclose v s ", ") 1 "two"
-- "(1, two)"
-- >>> runFmt (enclose (fmt "<!--") (fmt "-->") s) "an html comment"
-- "<!--an html comment-->"
enclose :: Semigroup m => Fmt m s2 c -> Fmt m a s1 -> Fmt m s1 s2 -> Fmt m a c
enclose pre suf f = pre % f % suf
{-# INLINE enclose #-}

-- @ 'tuple' 'd' 'd' :: 'LogFmt2' a 'Int' @
--
-- >>> runFmt (tuple d t) 1 "two"
-- "(1, two)"
tuple :: (Semigroup m, IsString m) => Fmt m s c -> Fmt m a s -> Fmt m a c
tuple f1 f2 = parens $ enclose f1 f2 ", "

-- | Add double quotes around the formatted item:
-- 
-- Use this to escape a string:
--
-- >>> runFmt ("He said it was based on " % quotes t' % ".") "science"
-- He said it was based on "science".
quotes :: (Semigroup m, IsString m) => Fmt m s a -> Fmt m s a
quotes = enclose (fmt "\"") (fmt "\"")
{-# INLINE quotes #-}

-- | Add single quotes around the formatted item:
--
-- >>> let obj = Just Nothing in format ("The object is: " % quotes' shown % ".") obj
-- "The object is: 'Just Nothing'."
quotes' :: (Semigroup m, IsString m) => Fmt m s a -> Fmt m s a
quotes' = enclose (fmt "'") (fmt "'")
{-# INLINE quotes' #-}

-- | Add parentheses around the formatted item:
--
-- >>> runFmt ("We found " % parens d % " discrepancies.") 17
-- "We found (17) discrepancies."
--
-- >>> printf (get 5 (list (parens d))) [1..]
-- [(1), (2), (3), (4), (5)]
parens :: (Semigroup m, IsString m) => Fmt m s a -> Fmt m s a
parens = enclose (fmt "(") (fmt ")")
{-# INLINE parens #-}

-- | Add angle brackets around the formatted item:
--
-- >>> runFmt (angles c) '/'
-- "</>"
--
-- >>> format (list (angles t)) ["html", "head", "title", "body", "div", "span"]
-- "[<html>, <head>, <title>, <body>, <div>, <span>]"
angles :: (Semigroup m, IsString m) => Fmt m s a -> Fmt m s a
angles = enclose (fmt "<") (fmt ">")
{-# INLINE angles #-}

-- | Add curly brackets around the formatted item:
--
-- >>> runFmt ("\\begin" % braces t) "section"
-- "\\begin{section}"
braces :: (Semigroup m, IsString m) => Fmt m s a -> Fmt m s a
braces = enclose (fmt "{") (fmt "}")
{-# INLINE braces #-}

-- | Add square brackets around the formatted item:
--
-- >>> runFmt (brackets d) 7
-- "[7]"
brackets :: (Semigroup m, IsString m) => Fmt m s a -> Fmt m s a
brackets = enclose (fmt "[") (fmt "]")
{-# INLINE brackets #-}

-- | Add backticks around the formatted item:
--
-- >>> format ("Be sure to run " % backticks builder % " as root.") ":(){:|:&};:"
-- "Be sure to run `:(){:|:&};:` as root."
backticks :: (Semigroup m, IsString m) => Fmt m s a -> Fmt m s a
backticks = enclose (fmt "`") (fmt "`")
{-# INLINE backticks #-}



-- Collections

-------------------------


-- | Format each value in a list with spaces in between:
--
-- >>> format (hsep d) [1, 2, 3]
-- "1 2 3"
hsep :: Foldable f => Fmt1 B B a -> Fmt1 B s (f a)
hsep = intercalate " "
{-# INLINE hsep #-}

-- | Format each value in a list, placing each on its own line:
--
-- >>> printf (vsep c) ['a'..'c']
-- a
-- b
-- c
vsep :: Foldable f => Fmt1 B B a -> Fmt1 B s (f a)
vsep = catWith BL.unlines
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
hang :: Foldable t => Int64 -> Fmt1 B B a -> Fmt1 B s (t a)
hang n = vsep . indent n
{-# INLINE hang #-}

-- | Use the given text-joining function to join together the individually rendered items of a list.
--
-- >>> format (catWith (mconcat . reverse) d) [123, 456, 789]
-- "789456123"
catWith :: Foldable f => ([ByteString] -> ByteString) -> Fmt1 B B a -> Fmt1 B s (f a)
catWith join f = fmt1 $ BL.lazyByteString . join . fmap (BL.toLazyByteString . format f) . toList
{-# INLINABLE catWith #-}

-- | Format each value in a list and place the given string between each:
--
-- >>> docs = vsep d 
--  [1, 2, 3]
--
-- >>> format (takes 5 $ intercalate ", " d) [1..]
-- >>> format (takes 5 $ intercalate ", " d) [1..]
-- "1, 2, 3, 4, 5"
intercalate :: Foldable f => ByteString -> Fmt1 B B a -> Fmt1 B s (f a)
intercalate s = catWith (BL.intercalate s)
{-# INLINE intercalate #-}

-- | Add square brackets around the Foldable (e.g. a list), and separate each formatted item with a comma and space.
--
-- >>> format (list s) ["one", "two", "three"]
-- "[one, two, three]"
-- >>> printf (quotes $ list d) [1,2,3]
-- ["1", "2", "3"]
-- >>> printf (quotes $ list s) ["one", "two", "three"]
-- ["one", "two", "three"]
list :: Foldable f => Fmt1 B B a -> Fmt1 B s (f a)
list = intercalate ", " . brackets
{-# INLINE list #-}

jsonList :: Foldable f => Fmt1 B B a -> Fmt1 B s (f a)
jsonList f = fmt1 $ jsonListF (runFmt f)

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
yamlList :: Foldable f => Fmt1 B B a -> Fmt1 B s (f a)
yamlList f = fmt1 $ yamlListF (runFmt f)

{-# INLINE jsonMap #-}
jsonMap :: (IsList map, Item map ~ (k, v)) => Fmt1 B B k -> Fmt1 B B v -> Fmt1 B s map
jsonMap fk fv = fmt1 $ jsonMapF (runFmt fk) (runFmt fv)

{-# INLINE yamlMap #-}
yamlMap :: (IsList map, Item map ~ (k, v)) => Fmt1 B B k -> Fmt1 B B v -> Fmt1 B s map
yamlMap fk fv = fmt1 $ yamlMapF (runFmt fk) (runFmt fv)



-- Indentation

-------------------------

--  | Attach a name to a formatter.
--
-- >>> printf (name "clients" $ yamlList s) ["Alice", "Bob", "Zalgo"]
-- clients:
--   - Alice
--   - Bob
--   - Zalgo
name :: B -> Fmt B s a -> Fmt B s a
name b = refmt (nameF b)

-- | Utility for taking a text-splitting function and turning it into a formatting combinator.
--
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
splitWith
  :: (ByteString -> [ByteString]) -- ^ The text splitter
  -> (Fmt1 B s_ B -> Fmt1 B B [B]) -- ^ A list-formatting combinator, e.g. 'hsep', 'vsep', 'cat', etc.
  -> Fmt B s a -- ^ The base formatter, whose rendered text will be split
  -> Fmt B s a
splitWith split lf (Fmt g) = Fmt (g . (.f)) 
  where
    f = runFmt (lf $ fmt1 id) . fmap BL.lazyByteString . split . BL.toLazyByteString



-- Internal

-------------------------

nameF :: B -> B -> B
nameF k v = case BL.lines (BL.toLazyByteString v) of
    [] -> k <> ":\n"
    [l] -> k <> ": " <> BL.lazyByteString l <> "\n"
    ls ->
        k <> ":\n"
            <> mconcat ["  " <> BL.lazyByteString s <> "\n" | s <- ls]

{-# INLINE yamlListF #-}
{-# SPECIALIZE yamlListF :: (a -> B) -> [a] -> B #-}
yamlListF :: Foldable f => (a -> B) -> f a -> B
yamlListF fbuild xs = if null items then "[]\n" else mconcat items
  where
    bullet = "-"
    items = map buildItem (toList xs)
    spaces = BL.byteString $ mconcat $ replicate (B.length bullet + 1) (B.singleton ' ')
    newline = BL.byteString "\n"
    buildItem x = case BL.lines (BL.toLazyByteString (fbuild x)) of
      []     -> BL.byteString bullet <> newline
      (l:ls) -> BL.byteString bullet <> BL.byteString " " <> BL.lazyByteString l <> newline <>
                mconcat [spaces <> BL.lazyByteString s <> newline | s <- ls]


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
{-# INLINE jsonListF #-}
jsonListF :: Foldable f => (a -> B) -> f a -> B
jsonListF build xs
    | null items = "[]\n"
    | otherwise = "[\n" <> mconcat items <> "]\n"
  where
    items = zipWith buildItem (True : repeat False) (toList xs)
    -- Item builder
    --buildItem :: Bool -> a -> B
    
    buildItem isFirst x =
        case map BL.lazyByteString (BL.lines (BL.toLazyByteString (build x))) of
            []
                | isFirst -> "\n"
                | otherwise -> ",\n"
            (h : t) ->
                mconcat . map (<> "\n") $
                    if isFirst
                        then "  " <> h : fmap ("  " <>) t
                        else ", " <> h : fmap ("  " <>) t


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
jsonMapF :: (IsList map, Item map ~ (k, v)) => (k -> B) -> (v -> B) -> map -> B
jsonMapF fk fv xs
  | null items = "{}\n"
  | otherwise  = "{\n" <> mconcat items <> "}\n"
  where
    items = zipWith buildItem (True : repeat False) (IsList.toList xs)
    -- Item builder
    --buildItem :: Bool -> (k, v) -> B
    buildItem isFirst (k, v) = do
      let kb = (if isFirst then "  " else ", ") <> fk k
      case map BL.lazyByteString (BL.lines (BL.toLazyByteString (fv v))) of
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
{-# INLINE yamlMapF #-}
yamlMapF ::
    (IsList t, Item t ~ (k, v)) =>
    (k -> B) ->
    (v -> B) ->
    t ->
    B
yamlMapF fk fv xs
    | null items = "{}\n"
    | otherwise = mconcat items
  where
    items = map (\(k, v) -> nameF (fk k) (fv v)) (IsList.toList xs)

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

-- Ansi terminal formatters

-------------------------

code :: (Semigroup m, IsString m) => SGR -> Fmt m s a -> Fmt m s a
code = codes . pure

-- | Wrap content with escape sequence to set and reset color of normal intensity.
codes :: (Semigroup m, IsString m) => [SGR] -> Fmt m s a -> Fmt m s a
codes sgr x = enclose before after x
  where
    before = fromString $ setSGRCode sgr
    after = fromString $ setSGRCode [Reset]

erase :: (Semigroup m, IsString m) => Ordering -> Fmt m s a -> Fmt m s a
erase LT = suffix $ fromString clearFromCursorToLineBeginningCode
erase EQ = suffix $ fromString clearLineCode
erase GT = suffix $ fromString clearFromCursorToLineEndCode

reset :: (Semigroup m, IsString m) => Fmt m s a -> Fmt m s a
reset = prefix $ fromString $ setSGRCode [Reset]

shift :: (Semigroup m, IsString m) => Either Int Int -> Fmt m s a -> Fmt m s a
shift = prefix . fromString . either cursorBackwardCode cursorForwardCode

scroll :: (Semigroup m, IsString m) => Either Int Int -> Fmt m s a -> Fmt m s a
scroll = prefix . fromString . either scrollPageUpCode scrollPageDownCode

-- Emphasis

-------------------------

blink :: (Semigroup m, IsString m) => Fmt m s a -> Fmt m s a
blink = code $ SetBlinkSpeed SlowBlink

bold :: (Semigroup m, IsString m) => Fmt m s a -> Fmt m s a
bold = code $ SetConsoleIntensity BoldIntensity

italic :: (Semigroup m, IsString m) => Fmt m s a -> Fmt m s a
italic = code $ SetItalicized True

underline :: (Semigroup m, IsString m) => Fmt m s a -> Fmt m s a
underline = code $ SetUnderlining SingleUnderline

faint :: (Semigroup m, IsString m) => Fmt m s a -> Fmt m s a
faint = code $ SetConsoleIntensity FaintIntensity

-- Color

-------------------------

-- | The xterm < https://en.wikipedia.org/wiki/8-bit_color 8 bit > color encoding.
type XColor = Word8

-- | A simple palette consisting of a foreground and background color.
type Palette = (XColor, XColor)

dull :: (Semigroup m, IsString m) => Color -> ConsoleLayer -> Fmt m s a -> Fmt m s a
dull = layer . xtermSystem Dull

vivid :: (Semigroup m, IsString m) => Color -> ConsoleLayer -> Fmt m s a -> Fmt m s a
vivid = layer . xtermSystem Vivid

--vivid col lay = code $ SetColor lay Vivid col

layer :: (Semigroup m, IsString m) => XColor -> ConsoleLayer -> Fmt m s a -> Fmt m s a
layer pal lay = code $ SetPaletteColor lay pal

palette :: (Semigroup m, IsString m) => Palette -> Fmt m s a -> Fmt m s a
palette (fg, bg) = codes [SetPaletteColor Foreground fg, SetPaletteColor Background bg]

