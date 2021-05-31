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

    type Term,

    -- * Fmt
    Fmt (..),
    type Fmt1,
    type Fmt2,
    type Fmt3,

    runFmt,
    
    -- ** Introduction
    fmt,
    fmts,
    fmt1,
    fmt2,
    const1,
    const2,
    refmt,
    apply,
    bind,
    (%),

    -- * Formatters
    c,
    s,
    v,
    e,
    f,
    g,

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
import Data.Int (Int64)
import Data.Monoid
import Data.String
import qualified Data.List as L

--import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Control.Category as C
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BL
import GHC.Exts (IsList, Item)
import qualified GHC.Exts as IsList (toList)

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

runFmt :: Fmt m m a -> a
runFmt = flip unFmt id

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

-- Formatters

-------------------------

-- | Format a character.
c :: IsString m => Fmt1 m s Char
c = fmap (. pure) s
{-# INLINE c #-}

-- | Format a string.
s :: IsString m => Fmt1 m s String
s = fmt1 fromString
{-# INLINE s #-}

-- | Format a showable value.
--
-- Semantics are similar to 'ByteString.Printf.printf':
--
-- >>> ByteString.Printf.printf "%v" 42 :: String
-- "42"
-- >>> format v 42
-- "42"
v :: (IsString m, Show a) => Fmt1 m s a
v = fmt1 (fromString . show)
{-# INLINE v #-}

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

