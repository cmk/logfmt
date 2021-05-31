{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Internal format starters.
module Data.Fmt.Text (

    printf,
    format,
    remove,
    replace,
    reformat,
    
    -- * Formatters
    t,
    t',
    list,
    jsonList,
    yamlList,
    jsonMap,
    yamlMap,

    hsep,
    vsep,
    hang,
    catWith,
    intercalate,
    splitWith,
    
    -- ** Padding & Truncation
    name,
    fill,
    fill',
    preFill,
    preFill',
    abbrev,

    

) where

import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy (Text)
import Data.Fmt
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.Maybe
import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Data.String
import GHC.Exts (IsList, Item)
import System.IO
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.List as L
import qualified GHC.Exts as IsList (toList)
import qualified Numeric as N -- (showEFloat,showFFloat,showIntAtBase)
import Data.Text.Lazy.Builder.Int (decimal, hexadecimal)
import Data.Text.Lazy.Builder.RealFloat as TL
import qualified Data.Text.Lazy.IO as TL

type B = Builder

-- Formatters

-------------------------

-- | Format a lazy byte string.
t :: Fmt1 TL.Builder s Text
t = fmt1 TL.fromLazyText
{-# INLINE t #-}

-- | Format a strict byte string.
--
-- @ 'fmap' (. 'Data.Text.pack') 't'' :: 'Fmt1' 'Data.Text.Builder.Builder' s 'String' @
t' :: Fmt1 TL.Builder s T.Text
t' = fmt1 TL.fromText
{-# INLINE t' #-}

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

-- Formatters

-------------------------

-- | Run the formatter and print out the text to stdout.
printf :: Fmt Builder Term a -> a
printf = flip unFmt (TL.putStr . TL.toLazyText)

format :: IsString s => Fmt Builder s a -> a
format = flip unFmt (fromString . TL.unpack . TL.toLazyText)
{-# Specialize format :: Fmt Builder Text a -> a #-}
{-# Specialize format :: Fmt Builder T.Text a -> a #-}
{-# Specialize format :: Fmt Builder String a -> a #-}
{-# Specialize format :: Fmt Builder Builder a -> a #-}

-- | Filter the formatted string to not contain characters which pass the given predicate:
--
-- >>> format (remove Data.Char.isUpper t) "Data.Char.isUpper"
-- "ata.har.ispper"
remove :: (Char -> Bool) -> Fmt B s a -> Fmt B s a
remove p = reformat (TL.filter (not . p))
{-# INLINE remove #-}

-- | Take a formatter and replace the given needle with the given replacement in its output.
--
-- >>> format (replace "Bruce" "<redacted>" t') "Bruce replied that Bruce's name was, in fact, '<redacted>'."
-- "<redacted> replied that <redacted>'s name was, in fact, '<redacted>'."
replace :: Text -> Text -> Fmt Builder s a -> Fmt Builder s a
replace search replacement = reformat (TL.replace search replacement)
{-# INLINE replace #-}

-- | Alter the formatted string with the given function.
--
-- >>> format (reformat TL.reverse d) 123456
-- "654321"
reformat :: (Text -> Text) -> Fmt Builder s a -> Fmt Builder s a
reformat f = refmt (TL.fromLazyText . f . TL.toLazyText)
{-# INLINABLE reformat #-}

-- Padding & truncation

-------------------------

-- | Pad the formatted string on the right to give it the given minimum width:
--
-- >>> format (fill 7 d) 1
-- "1      "
fill :: Char -> Int64 -> Fmt Builder s a -> Fmt Builder s a
fill c i = reformat (TL.justifyLeft i c)
{-# INLINE fill #-}

-- | Format the item with a fixed width, padding or adding an ellipsis on the right.
--
-- >>> format (fill' ' ' 10 d) 123
-- "123       "
--
-- >>> format (fill' ' ' 10 d) 1234567890
-- "1234567890"
--
-- >>> format (fill' ' ' 10 d) 123456789012345
-- "1234567..."
fill' :: Char -> Int64 -> Fmt Builder s a -> Fmt Builder s a
fill' c i = f i . fill c i where f n = abbrev 0 (n - 3)
{-# INLINE fill' #-}

-- | Pad the formatted string on the left to give it the given minimum width:
--
-- >>> format (preFill ' ' 7 d) 1
-- "      1"
--
-- >>> format (preFill ' ' 7 d) 123456789
-- "123456789"
preFill :: Char -> Int64 -> Fmt Builder s a -> Fmt Builder s a
preFill c i = reformat (TL.justifyRight i c)
{-# INLINE preFill #-}

-- | Format the item with a fixed width, padding or adding an ellipsis on the left.
--
-- >>> format (preFill' 10 d) 123
-- "       123"
--
-- >>> format (preFill' ' ' 10 d) 1234567890
-- "1234567890"
--
-- >>> format (preFill' ' ' 10 d) 123456789012345
-- "...9012345"
preFill' :: Char -> Int64 -> Fmt Builder s a -> Fmt Builder s a
preFill' c i = f i . preFill c i where f n = abbrev (n - 3) 0
{-# INLINE preFill' #-}

-- | Abbreviate the formatted string, leaving the given number of characters at the start and end, and placing an ellipsis in between.
-- The length will be no longer than `start + end + 3` characters long.
-- 
-- >>> format (abbrev 15 4 t) "The quick brown fox jumps over the lazy dog."
-- "The quick brown...dog."
--
-- >>> format (abbrev 15 4 t) "The quick brown fox"
-- "The quick brown fox"
abbrev :: Int64 -> Int64 -> Fmt Builder s a -> Fmt Builder s a
abbrev start end = reformat shorten
  where
    shorten :: Text -> Text
    shorten txt =
      let n = start + end + 3
      in if TL.length txt <= n
        then txt
        else TL.take start txt <> "..." <> TL.takeEnd end txt
{-# INLINABLE abbrev #-}




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
vsep = catWith TL.unlines
{-# INLINE vsep #-}

-- | Format a list of items, placing one per line, indent by the given number of spaces.
--
-- @ 'indentEach' n = 'Test.Contra.Type.Format.vsep' . 'indent' n @
--
-- >>> printf (splitWith TL.lines (indentList 2) t) "one\ntwo\nthree"
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
catWith :: Foldable f => ([Text] -> Text) -> Fmt1 B B a -> Fmt1 B s (f a)
catWith join f = fmt1 $ TL.fromLazyText . join . fmap (TL.toLazyText . format f) . toList
{-# INLINABLE catWith #-}

-- | Format each value in a list and place the given string between each:
--
-- >>> docs = vsep d 
--  [1, 2, 3]
--
-- >>> format (takes 5 $ intercalate ", " d) [1..]
-- >>> format (takes 5 $ intercalate ", " d) [1..]
-- "1, 2, 3, 4, 5"
intercalate :: Foldable f => Text -> Fmt1 B B a -> Fmt1 B s (f a)
intercalate s = catWith (TL.intercalate s)
{-# INLINE intercalate #-}

-- | Utility for taking a text-splitting function and turning it into a formatting combinator.
--
-- >>> commas = reverse . fmap TL.reverse . TL.chunksOf 3 . TL.reverse
-- >>> dollars = prefix "$" . splitWith commas (intercalate ",") . reversed
-- >>> format (dollars d) 1234567890
-- "$1,234,567,890"
-- >>> printf (splitWith (TL.splitOn ",") vsep t) "one,two,three"
-- one
-- two
-- three
-- >>> printf (splitWith (TL.splitOn ",") (indentEach 4) t) "one,two,three"
--     one
--     two
--     three
{-# INLINABLE splitWith #-}
splitWith
  :: (Text -> [Text]) -- ^ The text splitter
  -> (Fmt1 B s_ B -> Fmt1 B B [B]) -- ^ A list-formatting combinator, e.g. 'hsep', 'vsep', 'cat', etc.
  -> Fmt B s a -- ^ The base formatter, whose rendered text will be split
  -> Fmt B s a
splitWith split lf (Fmt g) = Fmt (g . (.f)) 
  where
    f = runFmt (lf $ fmt1 id) . fmap TL.fromLazyText . split . TL.toLazyText


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



-- Internal

-------------------------

nameF :: B -> B -> B
nameF k v = case TL.lines (TL.toLazyText v) of
    [] -> k <> ":\n"
    [l] -> k <> ": " <> TL.fromLazyText l <> "\n"
    ls ->
        k <> ":\n"
            <> mconcat ["  " <> TL.fromLazyText s <> "\n" | s <- ls]

{-# INLINE yamlListF #-}
{-# SPECIALIZE yamlListF :: (a -> B) -> [a] -> B #-}
yamlListF :: Foldable f => (a -> B) -> f a -> B
yamlListF fbuild xs = if null items then "[]\n" else mconcat items
  where
    bullet = "-"
    items = map buildItem (toList xs)
    spaces = TL.fromText $ mconcat $ replicate (T.length bullet + 1) (T.singleton ' ')
    newline = TL.fromText "\n"
    buildItem x = case TL.lines (TL.toLazyText (fbuild x)) of
      []     -> TL.fromText bullet <> newline
      (l:ls) -> TL.fromText bullet <> TL.fromText " " <> TL.fromLazyText l <> newline <>
                mconcat [spaces <> TL.fromLazyText s <> newline | s <- ls]


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
        case map TL.fromLazyText (TL.lines (TL.toLazyText (build x))) of
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
      case map TL.fromLazyText (TL.lines (TL.toLazyText (fv v))) of
        []  -> kb <> ":\n"
        [l] -> kb <> ": " <> l <> "\n"
        ls  -> kb <> ":\n" <>
               mconcat ["    " <> s <> "\n" | s <- ls]

--  | A YAML-like map formatter:
--
-- >>> TL.putStrLn $ TL.toLazyText $ yamlMapF id id [("Odds", yamlListF (TL.fromString . show) "-" [1,3]), ("Evens", yamlListF (TL.fromString . show) "-" [2,4])]
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

-- Text formatters

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
    f n a = case TL.lines a of
              [] -> spaces n <> "\n"
              xs -> TL.unlines (map (spaces n <>) xs)
    spaces n = TL.replicate (fromIntegral n) (TL.singleton ' ')


--  | Add a prefix to the first line, and indent all lines but the first one.
--
-- The output will always end with a newline, even when the input doesn't.
indentF' :: Int -> T.Text -> B -> B
indentF' n pref a = case TL.lines (TL.toLazyText a) of
    [] -> TL.fromText pref <> "\n"
    (x : xs) ->
        TL.fromLazyText $
            TL.vsep $ (TL.fromStrict pref <> x) : map (spaces <>) xs
  where
    spaces = TL.replicate (fromIntegral n) (TL.singleton ' ')

--  |
-- Take the first N characters:
--
-- >>> prefixF 3 "hello"
-- "hel"
prefixF :: Buildable a => Int -> a -> B
prefixF size =
    TL.fromLazyText . TL.take (fromIntegral size) . TL.toLazyText . build

--  |
-- Take the last N characters:
--
-- >>> suffixF 3 "hello"
-- "llo"
suffixF :: Buildable a => Int -> a -> B
suffixF size =
    TL.fromLazyText
        . (\t -> TL.drop (TL.length t - fromIntegral size) t)
        . TL.toLazyText
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
        | "\n" `TL.isSuffixOf` TL.toLazyText x = x
        | otherwise = x <> "\n"
{-# SPECIALIZE vsepF :: Buildable a => [a] -> B #-}


-}
