module Data.Fmt.Code (
    v,

    -- * Character encodings
    c,
    c7,
    c8,
    s,
    s7,
    s8,

    -- * Ascii float encodings
    e,
    f,
    g,

    -- * Decimal encodings
    d,
    hhd,
    hd,
    ld,
    lld,
    u,
    hhu,
    hu,
    lu,
    llu,

    -- * Hexadecimal encodings
    x,
    hhx,
    hx,
    hx',
    lx,
    lx',
    llx,
    llx',

    -- * Binary encodings
    b,
    b',
    hhb,
    hb,
    hb',
    lb,
    lb',
    llb,
    llb',
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BL
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Fmt
import Data.Int
import Data.Word
import qualified Numeric as N

{- | Encode a loggable value.

 Semantics are similar to 'ByteString.Printf.printf':

 >>> Text.Printf.printf "%v" 42 :: String
 "42"
 >>> runLogFmt v 42
 "42"
-}
{-# INLINE v #-}
v :: ToLogStr a => Fmt1 LogStr s a
v = fmt1 toLogStr

-- Character encodings

-------------------------

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

{- | Format a floating point number to a given number of digits of precision.

 Semantics are similar to 'ByteString.Printf.printf':

 >>> ByteString.Printf.printf "%.5e" pi :: String
 "3.14159e0"
 >>> runLogFmt (e 5) pi
 "3.14159e0"
-}
e :: (IsString m, RealFloat a) => Int -> Fmt1 m s a
e prec = fmt1 $ fromString . flip (N.showEFloat $ Just prec) []

{- | Format a floating point number to a given number of digits of precision.

 Semantics are similar to 'ByteString.Printf.printf':

 >>> ByteString.Printf.printf "%.5f" maximal32 :: String
 "340282330000000000000000000000000000000.00000"
 >>> runLogFmt (f 5) maximal32
 "340282330000000000000000000000000000000.00000"
-}
f :: (IsString m, RealFloat a) => Int -> Fmt1 m s a
f prec = fmt1 $ fromString . flip (N.showFFloat $ Just prec) []

{- | Format a floating point number to a given number of digits of precision.

 Semantics are similar to 'ByteString.Printf.printf':

 >>> ByteString.Printf.printf "%.5g" maximal32 :: String
 "3.40282e38"
 >>> runLogFmt (g 5) maximal32
 "3.40282e38"
-}
g :: (IsString m, RealFloat a) => Int -> Fmt1 m s a
g prec = fmt1 $ fromString . flip (N.showGFloat $ Just prec) []

-- Decimal encodings

-------------------------

-- | Decimal encoding of an 'Int' using the ASCII digits.
{-# INLINE d #-}
d :: Fmt1 LogStr s Int
d = fmt1 $ toLogStr . BL.intDec

{- | Decimal encoding of an 'Int8' using the ASCII digits.

 e.g.

 > toLazyByteString (int8Dec 42)   = "42"
 > toLazyByteString (int8Dec (-1)) = "-1"
-}
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

-- Hexadecimal encodings
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

{- | Shortest hexadecimal encoding of a 'Word64' using lower-case characters.

 Semantics are similar to 'Text.Printf.printf':

 >>> P.printf "%s: %llx" "Val" (-7) :: String
 "Val: fffffffffffffff9"
 >>> printf (s % ": " % llx) "Val" (-7)
 "Val: fffffffffffffff9"
-}
{-# INLINE llx #-}
llx :: Fmt1 LogStr s Word64
llx = fmt1 $ toLogStr . BL.word64Hex

-- | Encode a 'Word64' using 16 nibbles.
{-# INLINE llx' #-}
llx' :: Fmt1 LogStr s Word64
llx' = fmt1 $ toLogStr . BL.word64HexFixed

-- Binary encodings
--------------------

-- | Format a lazy byte string.
b :: Fmt1 LogStr s BL.ByteString
b = fmt1 toLogStr
{-# INLINE b #-}

{- | Format a strict byte string.

 @ 'fmap' (. 'Data.ByteString.pack') 't'' :: 'Fmt1' 'Data.ByteString.Builder.Builder' s 'String' @
-}
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

{- | Encode a 'Word64' using little-endian format.

 Semantics are similar to 'Tebt.Printf.printf':

 >>> P.printf "%s: %llb" "Val" (-7) :: String
 "Val: fffffffffffffff9"
 >>> printf (s % ": " % llb) "Val" (-7)
 "Val: fffffffffffffff9"
-}
{-# INLINE llb #-}
llb :: Fmt1 LogStr s Word64
llb = fmt1 $ toLogStr . BL.word64LE

-- | Encode a 'Word64' using big-endian format.
{-# INLINE llb' #-}
llb' :: Fmt1 LogStr s Word64
llb' = fmt1 $ toLogStr . BL.word64BE
