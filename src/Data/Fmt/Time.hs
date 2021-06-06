{-# LANGUAGE OverloadedStrings #-}
module Data.Fmt.Time (
    time,
    date,
    euro,
    -- Time
    
    -- Intra-day
    hm,
    hms,
    second,
    minute,
    hour,
    
    -- Inter-day
    day,
    day',
    month,
    month',
    year,
    unix,
    zone,
    -- Duration
    secs,
    mins,
    hours,
    days,
    years,
) where

import Control.Arrow (arr)
import Data.Fmt.Type as Fmt --((%), fmt, fmt1, Fmt, Fmt1, runFmt)
--import qualified Data.Fmt.Type as Fmt
import Data.String
import Data.Time (FormatTime, formatTime, defaultTimeLocale)
import Data.Time.Format.ISO8601
import Prelude

--iso8601 :: (IsString m, Monoid m, ISO8601 a) => Fmt m a m
iso8601 :: ISO8601 a => Fmt LogStr a LogStr
iso8601 = arr $ fromString . iso8601Show

-- | A custom time formatter.
--
-- See "Data.Time.Format".
time :: (IsString m, FormatTime a) => String -> Fmt1 m s a
time s = fmt1 $ fromString . formatTime defaultTimeLocale s

-- | Local time: /05 Jun 2021 01:46:46 -0700/
date :: (IsString m, FormatTime a) => Fmt1 m s a
date = time "%d %b %Y %T %z"

-- | Local time (Euro convention): /2021-06-05 01:46:46 -0700/
euro :: (IsString m, FormatTime a) => Fmt1 m s a
euro = time "%Y-%m-%d %T %z"

-- now <- getZonedTime
-- printf (time "%a %b %e %H:%M:%S %EZ %Y") now
--

-- Time

-------------------------

-- | Time: @%m/%d/%y@.
--d :: (IsString m, FormatTime a) => Fmt1 m s a
--d = time "%D"

-- | Time: @%Y/%m/%d@.
--f :: (IsString m, FormatTime a) => Fmt1 m s a
--f = time "%F"




-- | Time: @%H:%M@.
hm :: (IsString m, FormatTime a) => Fmt1 m s a
hm = time "%R"

-- | Time: @%H:%M:%S@.
hms :: (IsString m, FormatTime a) => Fmt1 m s a
hms = time "%T"


-- Time units

-------------------------

-- | Second of minute: @00@ - @60@.
second :: (IsString m, FormatTime a) => Fmt1 m s a
second = time "%S"

-- | Minute of hour: @00@ - @59@.
minute :: (IsString m, FormatTime a) => Fmt1 m s a
minute = time "%M"

-- | Hour of day: @00@ - @23@.
hour :: (IsString m, FormatTime a) => Fmt1 m s a
hour = time "%H"

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

