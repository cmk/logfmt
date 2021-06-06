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
module Data.Fmt.Type (

    Term,

    -- * Fmt
    LogFmt,
    Fmt (..),
    fmt,
    logFmt,
    runFmt,
    format,
    printf,
    refmt,
    apply,
    bind,
    cat,
    (%),

    -- * Fmt1
    Fmt1,
    Fmt2,
    Fmt3,
    fmt1,
    fmt2,
    const1,
    const2,
    cat1,
    (.%),

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
    braces,
    brackets,
    backticks,

    -- * Formatters
    c,
    s,
    sh,
    e,
    f,
    g,

    -- ** Collections
    left1,
    right1,
    either1,
    maybe1,

    -- * Re-exports
    LogStr,
    fromLogStr,
    ToLogStr(..),
    IsString(..)
) where

--import Data.Printf
--import qualified Data.List as L
import Control.Applicative (Const (..), getConst)
import Control.Arrow
import Control.Category (Category (), (<<<), (>>>))
import Data.Bifunctor (bimap)
import Data.Function ((&))
import Data.Profunctor
import Data.String
import qualified Control.Category as C
import qualified Numeric as N -- (showEFloat,showFFloat,showIntAtBase)
import System.Log.FastLogger (LogStr, fromLogStr, ToLogStr(..))

import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BL
-- $setup
-- >>> import Test.Contra.Fmt


type Term = IO ()

type LogFmt = Fmt LogStr

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
newtype Fmt m a b = Fmt {unFmt :: (m -> a) -> b}

deriving via (Costar ((->) m) a) instance Functor (Fmt m a)
deriving via (Costar ((->) m) a) instance Applicative (Fmt m a)
deriving via (Costar ((->) m) a) instance Monad (Fmt m a)
deriving via (Costar ((->) m)) instance Profunctor (Fmt m)
deriving via (Costar ((->) m)) instance Closed (Fmt m)
deriving via (Costar ((->) m)) instance Costrong (Fmt m)
deriving via (Costar ((->) m)) instance Cochoice (Fmt m)

instance (IsString s, Show a) => Show (LogFmt s a) where
    show = show . format

instance Semigroup m => Semigroup (Fmt1 m s a) where
    (<>) = (.%)

instance Monoid m => Monoid (Fmt1 m a b) where
    mempty = Fmt (\k _ -> k mempty)

instance (IsString m, a ~ b) => IsString (Fmt m a b) where
    fromString = fmt . fromString

instance Monoid m => Category (Fmt m) where
    id = fmt mempty
    (.) = (%)

instance Monoid m => Arrow (Fmt m) where
    arr f = Fmt $ \k -> f (k mempty)
    x *** y = dimap fst (,) x <*> lmap snd y

{- TODO: check whether this is lawful
instance Monoid m => ArrowChoice (Fmt m) where
    x +++ y = Fmt $ bimap (unFmt x) (unFmt y) . coapply

coapply :: Monoid m => (m -> Either a b) -> Either (m -> a) (m -> b)
coapply f = either (Left . const) (Right . const) (f mempty)
-}


-- | Format a constant value of type @m@.
--
fmt :: m -> Fmt m a a
fmt m = Fmt ($ m)

logFmt :: ToLogStr m => m -> Fmt LogStr a a
logFmt = fmt . toLogStr

runFmt :: Fmt m m a -> a
runFmt = flip unFmt id

format :: IsString s => Fmt LogStr s a -> a
format = flip unFmt (fromString . B.unpack . fromLogStr)
{-# Specialize format :: Fmt LogStr BL.ByteString a -> a #-}
{-# Specialize format :: Fmt LogStr ByteString a -> a #-}
{-# Specialize format :: Fmt LogStr String a -> a #-}
{-# Specialize format :: Fmt LogStr LogStr a -> a #-}
{-# Specialize format :: Fmt LogStr Builder a -> a #-}

-- | Run the formatter and print out the text to stdout.
--printf :: Fmt LogStr Term a -> a
--printf = flip unFmt (B.putStr . fromLogStr)

printf :: Fmt LogStr Term a -> a
printf = flip unFmt (B.putStrLn . fromLogStr)

-- | Map over the the formatting @Monoid@.
--
refmt :: (m1 -> m2) -> Fmt m1 a b -> Fmt m2 a b
refmt m12 (Fmt f) = Fmt $ \a -> f (a . m12)

apply :: Fmt1 m s m -> Fmt m s a -> Fmt m s a
apply (Fmt f) (Fmt a) = Fmt (a . f)

-- | Indexed bind.
--
bind :: Fmt m a1 b -> (m -> Fmt m a2 a1) -> Fmt m a2 b
bind m f = Fmt $ \k -> unFmt m (\a -> unFmt (f a) k)

cat :: (Monoid m, Foldable f) => f (Fmt m a a) -> Fmt m a a
cat = foldr (%) C.id
{-# INLINE cat #-}

-- | Concatenate two formatters.
--
infixr 6 %
(%) :: Semigroup m => Fmt m b c -> Fmt m a b -> Fmt m a c
f % g =
        f
          `bind` \a ->
              g
                  `bind` \b -> fmt (a <> b)

-- Fmt1

-------------------------

-- | A unary higher-order formatter.
--
-- @ 'Fmt1' m s a ~ (m -> s) -> a -> s @
type Fmt1 m s a = Fmt m s (a -> s)

-- | A binary higher-order formatter.
--
-- @ 'Fmt2' m s a b ~ (m -> s) -> a -> b -> s @
type Fmt2 m s a b = Fmt m s (a -> b -> s)

-- | A ternary higher-order formatter.
--
-- @ 'Fmt3' m s a b c ~ (m -> s) -> a -> b -> c -> s @
type Fmt3 m s a b c = Fmt m s (a -> b -> c -> s)

-- | Format a value of type @a@ using a function of type @a -> m@.
--
-- @ 'runFmt' . 'fmt1' :: (a -> m) -> a -> m @
--
fmt1 :: (a -> m) -> Fmt1 m s a
fmt1 f = Fmt $ \k -> k . f

fmt2 :: (a -> b -> m) -> Fmt2 m s a b
fmt2 f = Fmt $ \k -> fmap k . f

const1 :: Fmt m a a -> Fmt1 m a b
const1 = lmap const . closed

const2 :: Fmt m a a -> Fmt2 m a b c
const2 = lmap (const.const) . (closed.closed)

-- | Format each value in a list and concatenate them all:
--
-- >>> runFmt (cat1 (s % " ")) ["one", "two", "three"]
-- "one two three "
--
cat1 :: (Monoid m, Foldable f) => Fmt1 m m a -> Fmt1 m s (f a)
cat1 f = fmt1 $ foldMap (runFmt f)
{-# INLINE cat1 #-}

-- | Concatenate two formatters, applying both to the same input.
--
infixr 6 .%
(.%) :: Semigroup m => Fmt1 m s a -> Fmt1 m s a -> Fmt1 m s a
m .% n =
        Fmt
            ( \k a ->
                unFmt m (\b1 -> unFmt n (\b2 -> k (b1 <> b2)) a) a
            )

-- Formatting

-------------------------

-- | A string consisting of /n/ spaces.
spaces :: IsString m => Int -> m
spaces n = fromString $ replicate n ' '

-- | Insert the given number of spaces at the start of the rendered text:
--
-- >>> runFmt (indent 4 d) 7
-- "    7"
--
-- Note that this only indents the first line of a multi-line string.
-- To indent all lines see 'reindent'.
indent :: (IsString m, Semigroup m) => Int -> Fmt m a b -> Fmt m a b
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
prefix :: Semigroup m => m -> Fmt m a b -> Fmt m a b
prefix s f = fmt s % f
{-# INLINE prefix #-}

-- | Add the given suffix to the formatted item.
suffix :: Semigroup m => m -> Fmt m a b -> Fmt m a b
suffix s f = f % fmt s
{-# INLINE suffix #-}

-- | Enclose the output string with the given strings:
--
-- >>> runFmt (parens $ enclose v s ", ") 1 "two"
-- "(1, two)"
-- >>> runFmt (enclose (fmt "<!--") (fmt "-->") s) "an html comment"
-- "<!--an html comment-->"
enclose :: Semigroup m => Fmt m b2 c -> Fmt m a b1 -> Fmt m b1 b2 -> Fmt m a c
enclose pre suf f = pre % f % suf
{-# INLINE enclose #-}

-- @ 'tuple' 'd' 'd' :: 'LogFmt2' a 'Int' @
--
-- >>> runFmt (tuple d t) 1 "two"
-- "(1, two)"
tuple :: (Semigroup m, IsString m) => Fmt m b c -> Fmt m a b -> Fmt m a c
tuple f1 f2 = parens $ enclose f1 f2 ", "

-- | Add double quotes around the formatted item:
-- 
-- Use this to escape a string:
--
-- >>> runFmt ("He said it was based on " % quotes t' % ".") "science"
-- He said it was based on "science".
quotes :: (Semigroup m, IsString m) => Fmt m a b -> Fmt m a b
quotes = enclose "\"" "\""
{-# INLINE quotes #-}

-- | Add single quotes around the formatted item:
--
-- >>> let obj = Just Nothing in format ("The object is: " % quotes' shown % ".") obj
-- "The object is: 'Just Nothing'."
quotes' :: (Semigroup m, IsString m) => Fmt m a b -> Fmt m a b
quotes' = enclose "'" "'"
{-# INLINE quotes' #-}

-- | Add parentheses around the formatted item:
--
-- >>> runFmt ("We found " % parens d % " discrepancies.") 17
-- "We found (17) discrepancies."
--
-- >>> printf (get 5 (list (parens d))) [1..]
-- [(1), (2), (3), (4), (5)]
parens :: (Semigroup m, IsString m) => Fmt m a b -> Fmt m a b
parens = enclose "(" ")"
{-# INLINE parens #-}

-- | Add braces around the formatted item:
--
-- >>> runFmt ("\\begin" % braces t) "section"
-- "\\begin{section}"
braces :: (Semigroup m, IsString m) => Fmt m a b -> Fmt m a b
braces = enclose "{" "}"
{-# INLINE braces #-}

-- | Add square brackets around the formatted item:
--
-- >>> runFmt (brackets d) 7
-- "[7]"
brackets :: (Semigroup m, IsString m) => Fmt m a b -> Fmt m a b
brackets = enclose "[" "]"
{-# INLINE brackets #-}

-- | Add backticks around the formatted item:
--
-- >>> format ("Be sure to run " % backticks builder % " as root.") ":(){:|:&};:"
-- "Be sure to run `:(){:|:&};:` as root."
backticks :: (Semigroup m, IsString m) => Fmt m a b -> Fmt m a b
backticks = enclose "`" "`"
{-# INLINE backticks #-}

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
{-# INLINE sh #-}
sh :: (IsString m, Show a) => Fmt1 m s a
sh = fmt1 (fromString . show)

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

-- | Render the value in a Left with the given formatter, rendering a Right as an empty string:
--
-- >>> format (left1 text) (Left "bingo")
-- "bingo"
--
-- >>> format (left1 text) (Right 16)
-- ""
left1 :: IsString m => Fmt1 m m a -> Fmt1 m s (Either a b)
left1 f = either1 f (fmt1 $ const "")
{-# INLINE left1 #-}

-- | Render the value in a Right with the given formatter, rendering a Left as an empty string:
--
-- >>> format (right1 text) (Left 16)
-- ""
--
-- >>> format (right1 text) (Right "bingo")
-- "bingo"
right1 :: IsString m => Fmt1 m m b -> Fmt1 m s (Either a b)
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

