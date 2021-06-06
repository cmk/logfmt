{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Fmt.Time (
    fmtTime,
    -- Time
    c,
    f,
    t,
    ep,
    ez,
    z,
    -- Time units
    s,
    m,
    h,
    p,
    w,
    d,
    b,
    y
    -- Duration
    ss,
    mm,
    hh,
    dd,
    yy,
) where

import Data.Fmt.Type as Fmt ((%), fmt, fmt1, Fmt1, runFmt, IsString(..))
import qualified Data.Fmt.Type as Fmt
import Data.List (find)
import Data.Tuple
import Data.Time (FormatTime, formatTime, defaultTimeLocale)


-- | A custom time formatter.
--
-- See "Data.Time.Format".
fmtTime :: (IsString m, FormatTime a) => String -> Fmt1 m s a
fmtTime s = fmt1 $ fromString . formatTime defaultTimeLocale s

-- Time

-------------------------

-- | Local time as 'dateTimeFmt' @locale@ (e.g. @%a %b %e %H:%M:%S %Z %Y@).
c :: (IsString m, FormatTime a) => Fmt1 m s a
c = fmtTime "%c"

-- | Time: @%Y-%m-%d@.
f :: (IsString m, FormatTime a) => Fmt1 m s a
f = fmtTime "%F"

-- | Time: @%H:%M@.
r :: (IsString m, FormatTime a) => Fmt1 m s a
r = fmtTime "%R"

-- | Time: @%H:%M:%S@.
t :: (IsString m, FormatTime a) => Fmt1 m s a
t = fmtTime "%T"

-- | Seconds from Unix epoch.
ep :: (IsString m, FormatTime a) => Fmt1 m s a
ep = fmtTime "%s"

-- | Timezone offset: @-HH:MM@.
ez :: (IsString m, FormatTime a) => Fmt1 m s a
ez = fmtTime "%Ez"

-- | Timezone offset: @-HHMM@.
z :: (IsString m, FormatTime a) => Fmt1 m s a
z = fmtTime "%z"

-- Time units

-------------------------

-- | Second of minute: @00@ - @60@.
s :: (IsString m, FormatTime a) => Fmt1 m s a
s = fmtTime "%S"

-- | Minute of hour: @00@ - @59@.
m :: (IsString m, FormatTime a) => Fmt1 m s a
m = fmtTime "%M"

-- | Hour of day: @00@ - @23@.
h :: (IsString m, FormatTime a) => Fmt1 m s a
h = fmtTime "%H"

-- | Day half from ('amPm' @locale@), @AM@ - @PM@.
p :: (IsString m, FormatTime a) => Fmt1 m s a
p = fmtTime "%p"

-- | Day of week: @Sun@ - @Sat@.
w :: (IsString m, FormatTime a) => Fmt1 m s a
w = fmtTime "%a"

-- | Day of month: @01@ - @31@.
d :: (IsString m, FormatTime a) => Fmt1 m s a
d = fmtTime "%d"

-- | Month of year: @Jan@ - @Dec@.
b :: (IsString m, FormatTime a) => Fmt1 m s a
b = fmtTime "%b"

-- | Year.
y :: (IsString m, FormatTime a) => Fmt1 m s a
y = fmtTime "%Y"


-- Duration

-------------------------

-- | Display to a given precision the absolute value time span in seconds.
ss :: IsString m => Int -> Fmt1 m r Double
ss n = fmt1 (runFmt (Fmt.f n) . abs . count) where count n' = n'

-- | Display to a given precision the absolute value time span in minutes.
mm :: IsString m => Int -> Fmt1 m r Double
mm n = fmt1 (runFmt (Fmt.f n) . abs . count) where count n' = n' / 60

-- | Display to a given precision the absolute value time span in hours.
hh :: IsString m => Int -> Fmt1 m r Double
hh n = fmt1 (runFmt (Fmt.f n) . abs . count) where count n' = n' / 60 / 60

-- | Display to a given precision the absolute value time span in days.
dd :: (IsString m) => Int -> Fmt1 m r Double
dd n = fmt1 (runFmt (Fmt.f n) . abs . count) where count n' = n' / 24 / 60 / 60

-- | Display to a given precision the absolute value time span in years.
yy :: IsString m => Int -> Fmt1 m r Double
yy n = fmt1 (runFmt (Fmt.f n) . abs . count) where count n' = n' / 365 / 24 / 60 / 60

