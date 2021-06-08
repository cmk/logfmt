{-# LANGUAGE OverloadedStrings #-}

module Data.Fmt.Time (
    time,

    -- * Time
    t,
    hm,
    hms,
    day,
    day',
    month,
    month',
    year,
    unix,
    zone,

    -- * Duration
    secs,
    mins,
    hours,
    days,
    years,
) where

import Data.Fmt
import Data.String
import Data.Time (FormatTime, defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (ISO8601, iso8601Show)
import Prelude hiding (min)
import qualified Data.Fmt.Code as Fmt

{- | A custom time formatter.

 For example, the E.U. formatting convention is @ 'time' "%Y-%m-%d %T %z" @
-}
time :: (IsString m, FormatTime a) => String -> Fmt1 m s a
time s = fmt1 $ fromString . formatTime defaultTimeLocale s

-- Time

-------------------------

-- | < https://en.wikipedia.org/wiki/ISO_8601 ISO-8601> time: /2021-06-05T01:46:46.173677-07:00/
t :: (IsString m, ISO8601 a) => Fmt1 m s a
t = fmt1 $ fromString . iso8601Show

-- | Hour-minute of day: @%H:%M@.
hm :: (IsString m, FormatTime a) => Fmt1 m s a
hm = time "%R"

-- | Hour-minute-second of day: @%H:%M:%S@.
hms :: (IsString m, FormatTime a) => Fmt1 m s a
hms = time "%T"

-- | Day of month: @01@ - @31@.
day :: (IsString m, FormatTime a) => Fmt1 m s a
day = time "%d"

-- | Day of week: @Sun@ - @Sat@.
day' :: (IsString m, FormatTime a) => Fmt1 m s a
day' = time "%a"

-- | Month of year: @01@ - @12@.
month :: (IsString m, FormatTime a) => Fmt1 m s a
month = time "%m"

-- | Month of year: @Jan@ - @Dec@.
month' :: (IsString m, FormatTime a) => Fmt1 m s a
month' = time "%b"

-- | Year.
year :: (IsString m, FormatTime a) => Fmt1 m s a
year = time "%Y"

-- | Seconds from Unix epoch.
unix :: (IsString m, FormatTime a) => Fmt1 m s a
unix = time "%s"

-- | Timezone offset: @-HHMM@.
zone :: (IsString m, FormatTime a) => Fmt1 m s a
zone = time "%z"

-- Duration

-------------------------

-- | Display to a given precision the absolute value time span in seconds.
secs :: IsString m => Int -> Fmt1 m s Double
secs n = fmt1 (runFmt (Fmt.f n) . abs . count) where count n' = n'

-- | Display to a given precision the absolute value time span in minutes.
mins :: IsString m => Int -> Fmt1 m s Double
mins n = fmt1 (runFmt (Fmt.f n) . abs . count) where count n' = n' / 60

-- | Display to a given precision the absolute value time span in hours.
hours :: IsString m => Int -> Fmt1 m s Double
hours n = fmt1 (runFmt (Fmt.f n) . abs . count) where count n' = n' / 60 / 60

-- | Display to a given precision the absolute value time span in days.
days :: (IsString m) => Int -> Fmt1 m s Double
days n = fmt1 (runFmt (Fmt.f n) . abs . count) where count n' = n' / 24 / 60 / 60

-- | Display to a given precision the absolute value time span in years.
years :: IsString m => Int -> Fmt1 m s Double
years n = fmt1 (runFmt (Fmt.f n) . abs . count) where count n' = n' / 365 / 24 / 60 / 60
