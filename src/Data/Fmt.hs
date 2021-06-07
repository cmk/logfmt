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

module Data.Fmt (

    Term,
    
    -- * Type
    LogFmt,
    Fmt (..),
    runFmt,
    runLogFmt,
    printf,

    -- * Fmt
    fmt,
    logFmt,
    (%),
    apply,
    bind,
    cat,
    refmt,
    replace1,
    splitWith,

    -- * Fmt1
    Fmt1,
    Fmt2,
    fmt1,
    fmt2,
    fmt1_,
    fmt2_,
    (.%),
    cat1,
    cat1With,
    split1With,

    -- * Formatting
    hsep,
    vsep,
    hang,
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

    -- * Collections
    left1,
    right1,
    either1,
    maybe1,
    list1,
    jsonList,
    yamlList,
    jsonMap,
    yamlMap,

    -- * Formatters
    v,

    -- ** Char
    c,
    c7,
    c8,
    
    -- ** String
    s,
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

    -- * Re-exports
    LogStr,
    fromLogStr,
    ToLogStr(..),
    IsString(..)
) where

import Control.Applicative (Const (..), getConst)
import Control.Arrow
import Control.Category (Category (), (<<<), (>>>))
import Data.Bifunctor (bimap)
import Data.Function ((&))
import Data.Foldable (toList)
import Data.Profunctor
import Data.String
import Data.Int
import Data.Word
import GHC.Exts (IsList, Item)
import qualified GHC.Exts as IsList (toList)
import qualified Control.Category as C
import qualified Numeric as N -- (showEFloat,showFFloat,showIntAtBase)
import System.Log.FastLogger (LogStr, fromLogStr, ToLogStr(..))

import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BL

-- $setup
-- >>> import Data.Printf
-- >>> :load Data.Fmt


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

instance (IsString s, Show a) => Show (Fmt LogStr s a) where
    show = show . runLogFmt

instance (IsString m, a ~ b) => IsString (Fmt m a b) where
    fromString = fmt . fromString

instance Semigroup m => Semigroup (Fmt1 m s a) where
    (<>) = (.%)

instance Monoid m => Monoid (Fmt1 m a b) where
    mempty = Fmt (\k _ -> k mempty)

instance Monoid m => Category (Fmt m) where
    id = fmt mempty
    (.) = (%)

instance Monoid m => Arrow (Fmt m) where
    arr f = Fmt $ \k -> f (k mempty)
    x *** y = dimap fst (,) x <*> lmap snd y

instance Monoid m => Strong (Fmt m) where
    first' x = x *** C.id
    second' x = C.id *** x

-- | Run a 'Fmt'.
{-# INLINE runFmt #-}
runFmt :: Fmt m m a -> a
runFmt = flip unFmt id

-- | Run a 'LogFmt'.
{-# INLINE runLogFmt #-}
runLogFmt :: IsString s => Fmt LogStr s a -> a
runLogFmt = flip unFmt (fromString . B.unpack . fromLogStr)
{-# Specialize runLogFmt :: Fmt LogStr BL.ByteString a -> a #-}
{-# Specialize runLogFmt :: Fmt LogStr ByteString a -> a #-}
{-# Specialize runLogFmt :: Fmt LogStr String a -> a #-}
{-# Specialize runLogFmt :: Fmt LogStr LogStr a -> a #-}
{-# Specialize runLogFmt :: Fmt LogStr Builder a -> a #-}

-- | Run a formatter and print out the text to stdout.
{-# INLINE printf #-}
printf :: Fmt LogStr Term a -> a
printf = flip unFmt (B.putStrLn . fromLogStr)

-- | Format a constant value of type @m@.
{-# INLINE fmt #-}
fmt :: m -> Fmt m a a
fmt m = Fmt ($ m)

-- | Format a constant value of type @m@.
{-# INLINE logFmt #-}
logFmt :: ToLogStr m => m -> Fmt LogStr a a
logFmt = fmt . toLogStr

-- | Concatenate two formatters.
--
infixr 6 %
{-# INLINE (%) #-}
(%) :: Semigroup m => Fmt m b c -> Fmt m a b -> Fmt m a c
f % g =
        f
          `bind` \a ->
              g
                  `bind` \b -> fmt (a <> b)

-- | Apply a 'Fmt1' to a 'Fmt'.
{-# INLINE apply #-}
apply :: Fmt1 m s m -> Fmt m s a -> Fmt m s a
apply (Fmt f) (Fmt a) = Fmt (a . f)

-- | Indexed bind.
{-# INLINE bind #-}
bind :: Fmt m a1 b -> (m -> Fmt m a2 a1) -> Fmt m a2 b
bind m f = Fmt $ \k -> unFmt m (\a -> unFmt (f a) k)

-- | Concatenate a collection of formatters.
{-# INLINE cat #-}
cat :: (Monoid m, Foldable f) => f (Fmt m a a) -> Fmt m a a
cat = foldr (%) C.id

-- | Map over the the formatting @Monoid@.
--
{-# INLINE refmt #-}
refmt :: (m1 -> m2) -> Fmt m1 a b -> Fmt m2 a b
refmt m12 (Fmt f) = Fmt $ \a -> f (a . m12)

-- | Replace one occurance of a search term.
--
-- > replace1 "bar" "foo" "foobarbaz"
-- "foofoobaz"
{-# INLINE replace1 #-}
replace1 :: ByteString -> Fmt LogStr a a -> Fmt LogStr a b -> Fmt LogStr a b
replace1 x y =
    splitWith (B.breakSubstring x) $ \l r0 ->
        case B.stripPrefix x r0 of
            Nothing -> logFmt l
            Just r -> cat [logFmt l, y, logFmt r]

{-# INLINE splitWith #-}
splitWith ::
  (ByteString -> (ByteString, ByteString)) -> -- ^ Splitter
  (ByteString -> ByteString -> Fmt LogStr a2 a1) -> -- ^ Joiner
  Fmt LogStr a1 b ->
  Fmt LogStr a2 b
splitWith break join = flip bind $ uncurry join . break . fromLogStr

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
{-# INLINE fmt1 #-}
fmt1 :: (a -> m) -> Fmt1 m s a
fmt1 f = Fmt $ \k -> k . f

{-# INLINE fmt2 #-}
fmt2 :: (a -> b -> m) -> Fmt2 m s a b
fmt2 f = Fmt $ \k -> fmap k . f

{-# INLINE fmt1_ #-}
fmt1_ :: Fmt m a a -> Fmt1 m a b
fmt1_ = lmap const . closed

{-# INLINE fmt2_ #-}
fmt2_ :: Fmt m a a -> Fmt2 m a b c
fmt2_ = lmap (const.const) . (closed.closed)

-- | Concatenate two formatters, applying both to the same input.
--
infixr 6 .%
{-# INLINE (.%) #-}
(.%) :: Semigroup m => Fmt1 m s a -> Fmt1 m s a -> Fmt1 m s a
f .% g =
        Fmt
            ( \k a ->
                unFmt f (\b1 -> unFmt g (\b2 -> k (b1 <> b2)) a) a
            )

-- | Format each value in a list and concatenate them all:
--
-- >>> runFmt (cat1 (s % " ")) ["one", "two", "three"]
-- "one two three "
--
{-# INLINE cat1 #-}
cat1 :: (Monoid m, Foldable f) => Fmt1 m m a -> Fmt1 m s (f a)
cat1 f = fmt1 $ foldMap (runFmt f)

-- | Use the given text-joining function to join together the individually rendered items of a list.
--
-- >>> runLogFmt (cat1With (mconcat . reverse) d) [123, 456, 789]
-- "789456123"
--
-- @
-- 'cat1With' 'L.unlines' :: 'Foldable' f => 'Fmt1' 'LogStr' 'String' a -> 'Fmt1' 'LogStr' s (f a)
-- 'cat1With' 'T.unlines' :: 'Foldable' f => 'Fmt1' 'LogStr' 'T.Text' a -> 'Fmt1' 'LogStr' s (f a)
-- 'cat1With' 'B.unlines' :: 'Foldable' f => 'Fmt1' 'LogStr' 'B.ByteString' a -> 'Fmt1' 'LogStr' s (f a)
-- 'cat1With' '$' 'L.intercalate' " " :: 'Foldable' f => 'Fmt1' 'LogStr' 'String' a -> 'Fmt1' 'LogStr' s (f a)
-- 'cat1With' '$' 'T.intercalate' " " :: 'Foldable' f => 'Fmt1' 'LogStr' 'T.Text' a -> 'Fmt1' 'LogStr' s (f a)
-- 'cat1With' '$' 'B.intercalate' " " :: 'Foldable' f => 'Fmt1' 'LogStr' 'B.ByteString' a -> 'Fmt1' 'LogStr' s (f a)
-- @
{-# INLINABLE cat1With #-}
cat1With ::
  (Foldable f, ToLogStr str, IsString str) =>
  ([str] -> str) ->
  Fmt1 LogStr str a ->
  Fmt1 LogStr s (f a)
cat1With join f = fmt1 $ toLogStr . join . fmap (runLogFmt f) . toList

{-# Specialize cat1With :: Foldable f => ([LogStr] -> LogStr) -> Fmt1 LogStr LogStr a -> Fmt1 LogStr s (f a) #-}
{-# Specialize cat1With :: Foldable f => ([Builder] -> Builder) -> Fmt1 LogStr Builder a -> Fmt1 LogStr s (f a) #-}
{-# Specialize cat1With :: Foldable f => ([ByteString] -> ByteString) -> Fmt1 LogStr ByteString a -> Fmt1 LogStr s (f a) #-}
{-# Specialize cat1With :: Foldable f => ([BL.ByteString] -> BL.ByteString) -> Fmt1 LogStr BL.ByteString a -> Fmt1 LogStr s (f a) #-}

-- | Turn a text-splitting function into a formatting combinator.
--
-- @
--  'split1With' 'hsep' :: ('Traversable' f, 'ToLogStr' msg) => ('ByteString' -> f msg) -> 'Fmt' 'LogStr' s a -> 'Fmt' 'LogStr' s a
--  'split1With' 'vsep' :: ('Traversable' f, 'ToLogStr' msg) => ('ByteString' -> f msg) -> 'Fmt' 'LogStr' s a -> 'Fmt' 'LogStr' s a
--  'split1With' 'list1' :: ('Traversable' f, 'ToLogStr' msg) => ('ByteString' -> f msg) -> 'Fmt' 'LogStr' s a -> 'Fmt' 'LogStr' s a
-- @
-- >>> commas = reverse . fmap BL.reverse . BL.chunksOf 3 . BL.reverse
-- >>> dollars = prefix "$" . split1With commas (intercalate ",") . reversed
-- >>> runLogFmt (dollars d) 1234567890
-- "$1,234,567,890"
-- >>> printf (split1With (BL.splitOn ",") vsep t) "one,two,three"
-- one
-- two
-- three
-- >>> printf (split1With (BL.splitOn ",") (indentEach 4) t) "one,two,three"
--     one
--     two
--     three
{-# INLINABLE split1With #-}
split1With ::
  (Traversable f, ToLogStr str) =>
  (Fmt1 m s_ m -> Fmt1 m m (f LogStr)) ->
  (ByteString -> f str) ->
  Fmt LogStr s a ->
  Fmt m s a
split1With lf split (Fmt g) = Fmt (g . (. runFmt (lf $ fmt1 id) . fmap toLogStr . split . fromLogStr)) 

-- Formatting

-------------------------

-- | Format each value in a list with spaces in between:
--
-- >>> runLogFmt (hsep d) [1, 2, 3]
-- "1 2 3"
hsep :: Foldable f => Fmt1 LogStr ByteString a -> Fmt1 LogStr s (f a)
hsep = cat1With $ B.intercalate " "
{-# INLINE hsep #-}

-- | Format each value in a list, placing each on its own line:
--
-- >>> printf (vsep c) ['a'..'c']
-- a
-- b
-- c
vsep :: Foldable f => Fmt1 LogStr ByteString a -> Fmt1 LogStr s (f a)
vsep = cat1With B.unlines
{-# INLINE vsep #-}

-- | Format a list of items, placing one per line, indent by the given number of spaces.
--
-- @ 'indentEach' n = 'Test.Contra.Type.Format.vsep' . 'indent' n @
--
-- >>> printf (split1With BL.lines (indentList 2) t) "one\ntwo\nthree"
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

-- | Insert the given number of spaces at the start of the rendered text:
--
-- >>> runFmt (indent 4 d) 7
-- "    7"
--
-- Note that this only indents the first line of a multi-line string.
-- To indent all lines see 'reindent'.
indent :: (IsString m, Semigroup m) => Int -> Fmt m a b -> Fmt m a b
indent n = prefix $ fromString $ replicate n ' '
{-# INLINABLE indent #-}

-- | Add the given prefix to the formatted item:
--
-- >>> runLogFmt ("The answer is: " % prefix "wait for it... " d) 42
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
-- >>> printf (get 5 (list1 (parens d))) [1..]
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
-- >>> runLogFmt ("Be sure to run " % backticks builder % " as root.") ":(){:|:&};:"
-- "Be sure to run `:(){:|:&};:` as root."
backticks :: (Semigroup m, IsString m) => Fmt m a b -> Fmt m a b
backticks = enclose "`" "`"
{-# INLINE backticks #-}


-- Collections

-------------------------

-- | Render the value in a Left with the given formatter, rendering a Right as an empty string:
--
-- >>> runLogFmt (left1 text) (Left "bingo")
-- "bingo"
--
-- >>> runLogFmt (left1 text) (Right 16)
-- ""
left1 :: IsString m => Fmt1 m m a -> Fmt1 m s (Either a b)
left1 f = either1 f (fmt1 $ const "")
{-# INLINE left1 #-}

-- | Render the value in a Right with the given formatter, rendering a Left as an empty string:
--
-- >>> runLogFmt (right1 text) (Left 16)
-- ""
--
-- >>> runLogFmt (right1 text) (Right "bingo")
-- "bingo"
right1 :: IsString m => Fmt1 m m b -> Fmt1 m s (Either a b)
right1 = either1 (fmt1 $ const "")
{-# INLINE right1 #-}

-- | Render the value in an Either:
--
-- >>> runLogFmt (either1 text int) (Left "Error!"
-- "Error!"
--
-- >>> runLogFmt (either1 text int) (Right 69)
-- "69"
either1 :: Fmt1 m m a -> Fmt1 m m b -> Fmt1 m s (Either a b)
either1 l r = fmt1 $ either (runFmt l) (runFmt r)
{-# INLINE either1 #-}

-- | Render a Maybe value either as a default (if Nothing) or using the given formatter:
--
-- >>> runLogFmt (maybe1 "Goodbye" text) Nothing
-- "Goodbye"
--
-- >>> runLogFmt (maybe1 "Goodbye" text) (Just "Hello")
-- "Hello"
maybe1 :: m -> Fmt1 m m a -> Fmt1 m s (Maybe a)
maybe1 def f = fmt1 $ maybe def (runFmt f)
{-# INLINE maybe1 #-}

-- | Add square brackets around the Foldable (e.g. a list), and separate each formatted item with a comma and space.
--
-- >>> runLogFmt (list1 s) ["one", "two", "three"]
-- "[one, two, three]"
-- >>> printf (quotes $ list1 d) [1,2,3]
-- ["1", "2", "3"]
-- >>> printf (quotes $ list1 s) ["one", "two", "three"]
-- ["one", "two", "three"]
list1 :: Foldable f => Fmt1 LogStr ByteString a -> Fmt1 LogStr s (f a)
list1 = cat1With (B.intercalate ", ") . brackets
{-# INLINE list1 #-}

-- | A JSON-style formatter for lists.
--
-- >>> printf jsonList [1,2,3]
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


-- Formatters

-------------------------

-- | Encode a loggable value.
--
-- Semantics are similar to 'ByteString.Printf.printf':
--
-- >>> Text.Printf.printf "%v" 42 :: String
-- "42"
-- >>> runLogFmt v 42
-- "42"
{-# INLINE v #-}
v :: ToLogStr a => Fmt1 LogStr s a
v = fmt1 toLogStr

-- | Format a character.
{-# INLINE c #-}
c :: IsString m => Fmt1 m s Char
c = fmt1 (fromString . pure)

-- | ASCII encode a 'Char'.
{-# INLINE c7 #-}
c7 :: Fmt1 LogStr s Char
c7 = fmt1 $ toLogStr . BL.char7

-- | Latin-1 (ISO/IEC 8859-1) encode a 'Char'.
{-# INLINE c8 #-}
c8 :: Fmt1 LogStr s Char
c8 = fmt1 $ toLogStr . BL.char8

-- | Format a showable value.
{-# INLINE s #-}
s :: (IsString m, Show a) => Fmt1 m s a
s = fmt1 (fromString . show)

-- | ASCII encode a 'String'.
{-# INLINE s7 #-}
s7 :: Fmt1 LogStr s String
s7 = fmt1 $ toLogStr . BL.string7

-- | Latin-1 (ISO/IEC 8859-1) encode a 'String'.
{-# INLINE s8 #-}
s8 :: Fmt1 LogStr s String
s8 = fmt1 $ toLogStr . BL.string8

-- Floating point

-------------------------

-- | Format a floating point number to a given number of digits of precision.
--
-- Semantics are similar to 'ByteString.Printf.printf':
--
-- >>> ByteString.Printf.printf "%.5e" pi :: String
-- "3.14159e0"
-- >>> runLogFmt (e 5) pi
-- "3.14159e0"
e :: (IsString m, RealFloat a) => Int -> Fmt1 m s a
e prec = fmt1 $ fromString . flip (N.showEFloat $ Just prec) []

-- | Format a floating point number to a given number of digits of precision.
--
-- Semantics are similar to 'ByteString.Printf.printf':
--
-- >>> ByteString.Printf.printf "%.5f" maximal32 :: String
-- "340282330000000000000000000000000000000.00000"
-- >>> runLogFmt (f 5) maximal32
-- "340282330000000000000000000000000000000.00000"
f :: (IsString m, RealFloat a) => Int -> Fmt1 m s a
f prec = fmt1 $ fromString . flip (N.showFFloat $ Just prec) []

-- | Format a floating point number to a given number of digits of precision.
--
-- Semantics are similar to 'ByteString.Printf.printf':
--
-- >>> ByteString.Printf.printf "%.5g" maximal32 :: String
-- "3.40282e38"
-- >>> runLogFmt (g 5) maximal32
-- "3.40282e38"
g :: (IsString m, RealFloat a) => Int -> Fmt1 m s a
g prec = fmt1 $ fromString . flip (N.showGFloat $ Just prec) []

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
