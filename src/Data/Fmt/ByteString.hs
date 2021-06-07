{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Internal format starters.
module Data.Fmt.ByteString (
    -- * Format
    b,
    b',
    printf,
    format,
    remove,
    replace,
    reformat,

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
) where

import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BL
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Fmt
import Data.Foldable (toList)
import Data.Int (Int64)
import qualified Data.List as L
import Data.Maybe
import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Data.String
import GHC.Exts (IsList, Item)
import qualified GHC.Exts as IsList (toList)
import qualified Numeric as N -- (showEFloat,showFFloat,showIntAtBase)
import System.IO

type B = Builder

-- Formatters

-------------------------

-- | Format a lazy byte string.
b :: Fmt1 BL.Builder s ByteString
b = fmt1 BL.lazyByteString
{-# INLINE b #-}

{- | Format a strict byte string.

 @ 'fmap' (. 'Data.ByteString.pack') 't'' :: 'Fmt1' 'Data.ByteString.Builder.Builder' s 'String' @
-}
b' :: Fmt1 BL.Builder s B.ByteString
b' = fmt1 BL.byteString
{-# INLINE b' #-}

-- | Run the formatter and print out the text to stdout.
printf :: Fmt Builder Term a -> a
printf = flip unFmt (BL.putStr . BL.toLazyByteString)

format :: IsString s => Fmt Builder s a -> a
format = flip unFmt (fromString . BL.unpack . BL.toLazyByteString)
{-# SPECIALIZE format :: Fmt Builder ByteString a -> a #-}
{-# SPECIALIZE format :: Fmt Builder B.ByteString a -> a #-}
{-# SPECIALIZE format :: Fmt Builder String a -> a #-}
{-# SPECIALIZE format :: Fmt Builder Builder a -> a #-}

{- | Filter the formatted string to not contain characters which pass the given predicate:

 >>> format (remove Data.Char.isUpper t) "Data.Char.isUpper"
 "ata.har.ispper"
-}
remove :: (Char -> Bool) -> Fmt B s a -> Fmt B s a
remove p = reformat (BL.filter (not . p))
{-# INLINE remove #-}

{- | Filter the formatted string to contain only characters which pass the given predicate:

 >>> format (replace Data.Char.isUpper t) "Data.Char.isUpper"
 "DCU"
-}
replace :: Char -> (Char -> Bool) -> Fmt B s a -> Fmt B s a
replace c p = reformat (BL.map $ \x -> if p x then c else x)
{-# INLINE replace #-}

{- | Alter the formatted string with the given function.

 >>> format (reformat BL.reverse d) 123456
 "654321"
-}
reformat :: (ByteString -> ByteString) -> Fmt Builder s a -> Fmt Builder s a
reformat f = refmt (BL.lazyByteString . f . BL.toLazyByteString)
{-# INLINEABLE reformat #-}

-- Collections

-------------------------

{- | Format each value in a list with spaces in between:

 >>> format (hsep d) [1, 2, 3]
 "1 2 3"
-}
hsep :: Foldable f => Fmt1 B B a -> Fmt1 B s (f a)
hsep = intercalate " "
{-# INLINE hsep #-}

{- | Format each value in a list, placing each on its own line:

 >>> printf (vsep c) ['a'..'c']
 a
 b
 c
-}
vsep :: Foldable f => Fmt1 B B a -> Fmt1 B s (f a)
vsep = catWith BL.unlines
{-# INLINE vsep #-}

{- | Format a list of items, placing one per line, indent by the given number of spaces.

 @ 'indentEach' n = 'Test.Contra.Type.Format.vsep' . 'indent' n @

 >>> printf (splitWith BL.lines (indentList 2) t) "one\ntwo\nthree"
   one
   two
   three
 >>> printf ("The lucky numbers are:\n" % indentList 2 d) [7, 13, 1, 42]
 The lucky numbers are:
   7
   13
   1
   42
-}
hang :: Foldable t => Int64 -> Fmt1 B B a -> Fmt1 B s (t a)
hang n = vsep . indent n
{-# INLINE hang #-}

{- | Use the given text-joining function to join together the individually rendered items of a list.

 >>> format (catWith (mconcat . reverse) d) [123, 456, 789]
 "789456123"
-}
catWith :: Foldable f => ([ByteString] -> ByteString) -> Fmt1 B B a -> Fmt1 B s (f a)
catWith join f = fmt1 $ BL.lazyByteString . join . fmap (BL.toLazyByteString . format f) . toList
{-# INLINEABLE catWith #-}

{- | Format each value in a list and place the given string between each:

 >>> docs = vsep d
  [1, 2, 3]

 >>> format (takes 5 $ intercalate ", " d) [1..]
 >>> format (takes 5 $ intercalate ", " d) [1..]
 "1, 2, 3, 4, 5"
-}
intercalate :: Foldable f => ByteString -> Fmt1 B B a -> Fmt1 B s (f a)
intercalate s = catWith (BL.intercalate s)
{-# INLINE intercalate #-}

{- | Add square brackets around the Foldable (e.g. a list), and separate each formatted item with a comma and space.

 >>> format (list s) ["one", "two", "three"]
 "[one, two, three]"
 >>> printf (quotes $ list d) [1,2,3]
 ["1", "2", "3"]
 >>> printf (quotes $ list s) ["one", "two", "three"]
 ["one", "two", "three"]
-}
list :: Foldable f => Fmt1 B B a -> Fmt1 B s (f a)
list = intercalate ", " . brackets
{-# INLINE list #-}

jsonList :: Foldable f => Fmt1 B B a -> Fmt1 B s (f a)
jsonList f = fmt1 $ jsonListF (runFmt f)

{- | A multiline formatter for lists.

  >>> printf (yamlList d) [1,2,3]
  - 1
  - 2
  - 3

  Multi-line elements are indented correctly:

  >>> printf (yamlList s) ["hello\nworld", "foo\nbar\nquix"]
  - hello
    world
  - foo
    bar
    quix
-}
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

{- | Utility for taking a text-splitting function and turning it into a formatting combinator.

 >>> commas = reverse . fmap BL.reverse . BL.chunksOf 3 . BL.reverse
 >>> dollars = prefix "$" . splitWith commas (intercalate ",") . reversed
 >>> format (dollars d) 1234567890
 "$1,234,567,890"
 >>> printf (splitWith (BL.splitOn ",") vsep t) "one,two,three"
 one
 two
 three
 >>> printf (splitWith (BL.splitOn ",") (indentEach 4) t) "one,two,three"
     one
     two
     three
-}
{-# INLINEABLE splitWith #-}
splitWith ::
    -- | The text splitter
    (ByteString -> [ByteString]) ->
    -- | A list-formatting combinator, e.g. 'hsep', 'vsep', 'cat', etc.
    (Fmt1 B s_ B -> Fmt1 B B [B]) ->
    -- | The base formatter, whose rendered text will be split
    Fmt B s a ->
    Fmt B s a
splitWith split lf (Fmt g) = Fmt (g . (. f))
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
        [] -> BL.byteString bullet <> newline
        (l : ls) ->
            BL.byteString bullet <> BL.byteString " " <> BL.lazyByteString l <> newline
                <> mconcat [spaces <> BL.lazyByteString s <> newline | s <- ls]

{- | A JSON-style formatter for lists.

 >>> fmt $ jsonListF [1,2,3]
 [
   1
 , 2
 , 3
 ]

 Like 'yamlListF', it handles multiline elements well:

 >>> fmt $ jsonListF ["hello\nworld", "foo\nbar\nquix"]
 [
   hello
   world
 , foo
   bar
   quix
 ]
-}
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
    | otherwise = "{\n" <> mconcat items <> "}\n"
  where
    items = zipWith buildItem (True : repeat False) (IsList.toList xs)
    -- Item builder
    --buildItem :: Bool -> (k, v) -> B
    buildItem isFirst (k, v) = do
        let kb = (if isFirst then "  " else ", ") <> fk k
        case map BL.lazyByteString (BL.lines (BL.toLazyByteString (fv v))) of
            [] -> kb <> ":\n"
            [l] -> kb <> ": " <> l <> "\n"
            ls ->
                kb <> ":\n"
                    <> mconcat ["    " <> s <> "\n" | s <- ls]

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
