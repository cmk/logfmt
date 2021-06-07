module Data.Fmt.Ansi (
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

import Data.Fmt
import Data.Word
import System.Console.ANSI.Codes
import System.Console.ANSI.Types

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
