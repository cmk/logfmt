{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Fmt.Time
  ( tz
  , tzName
  , datetime
  , hm
  , hms
  , hmsL
  , hmsPL
  , dayHalf
  , dayHalfU
  , hour24
  , hour12
  , hour24S
  , hour12S
  , minute
  , second
  , pico
  , decimals
  , epoch
  , dateSlash
  , dateDash
  , dateSlashL
  , year
  , yy
  , century
  , monthName
  , monthNameShort
  , month
  , dayOfMonth
  , dayOfMonthOrd
  , dayOfMonthS
  , day
  , weekYear
  , weekYY
  , weekCentury
  , week
  , dayOfWeek
  , dayNameShort
  , dayName
  , weekFromZero
  , dayOfWeekFromZero
  , weekOfYearMon
  , diff
  , years
  , days
  , hours
  , minutes
  , seconds
  --, diffComponents
  --, customDiffComponents
  , customTimeFmt
  ) where

import Data.Fmt.Type as Fmt

import Data.List (find)
import Data.Tuple
import Data.Time (FormatTime, formatTime, defaultTimeLocale)
--import System.Locale ()
--import Control.Monad.Trans.State.Strict

-- * For 'TimeZone' (and 'ZonedTime' and 'UTCTime'):

-- | Timezone offset on the format @-HHMM@.
tz :: (IsString m, FormatTime a) => Fmt1 m r a
tz = fmt1 (fm "%z")

-- | Timezone name.
tzName :: (IsString m, FormatTime a) => Fmt1 m r a
tzName = fmt1 (fm "%Z")

-- | As 'dateTimeFmt' @locale@ (e.g. @%a %b %e %H:%M:%S %Z %Y@).
datetime :: (IsString m, FormatTime a) => Fmt1 m r a
datetime = fmt1 (fm "%c")

-- * For 'TimeOfDay' (and 'LocalTime' and 'ZonedTime' and 'UTCTime'):

-- | Same as @%H:%M@.
hm :: (IsString m, FormatTime a) => Fmt1 m r a
hm = fmt1 (fm "%R")

-- | Same as @%H:%M:%S@.
hms :: (IsString m, FormatTime a) => Fmt1 m r a
hms = fmt1 (fm "%T")

-- | As 'timeFmt' @locale@ (e.g. @%H:%M:%S@).
hmsL :: (IsString m, FormatTime a) => Fmt1 m r a
hmsL = fmt1 (fm "%X")

-- | As 'time12Fmt' @locale@ (e.g. @%I:%M:%S %p@).
hmsPL :: (IsString m, FormatTime a) => Fmt1 m r a
hmsPL = fmt1 (fm "%r")

-- | Day half from ('amPm' @locale@), converted to lowercase, @am@,
-- @pm@.
dayHalf :: (IsString m, FormatTime a) => Fmt1 m r a
dayHalf = fmt1 (fm "%P")

-- | Day half from ('amPm' @locale@), @AM@, @PM@.
dayHalfU :: (IsString m, FormatTime a) => Fmt1 m r a
dayHalfU = fmt1 (fm "%p")

-- | Hour, 24-hour, leading 0 as needed, @00@ - @23@.
hour24 :: (IsString m, FormatTime a) => Fmt1 m r a
hour24 = fmt1 (fm "%H")

-- | Hour, 12-hour, leading 0 as needed, @01@ - @12@.
hour12 :: (IsString m, FormatTime a) => Fmt1 m r a
hour12 = fmt1 (fm "%I")

-- | Hour, 24-hour, leading space as needed, @ 0@ - @23@.
hour24S :: (IsString m, FormatTime a) => Fmt1 m r a
hour24S = fmt1 (fm "%k")

-- | Hour, 12-hour, leading space as needed, @ 1@ - @12@.
hour12S :: (IsString m, FormatTime a) => Fmt1 m r a
hour12S = fmt1 (fm "%l")

-- | Minute, @00@ - @59@.
minute :: (IsString m, FormatTime a) => Fmt1 m r a
minute = fmt1 (fm "%M")

-- | Second, without decimal part, @00@ - @60@.
second :: (IsString m, FormatTime a) => Fmt1 m r a
second = fmt1 (fm "%S")

-- | Picosecond, including trailing zeros, @000000000000@ -
-- @999999999999@.
pico :: (IsString m, FormatTime a) => Fmt1 m r a
pico = fmt1 (fm "%q")

-- | Decimal point and up to 12 second decimals, without trailing
-- zeros. For a whole number of seconds, this produces the empty
-- string.
decimals :: (IsString m, FormatTime a) => Fmt1 m r a
decimals = fmt1 (fm "%Q")

-- * For 'UTCTime' and 'ZonedTime'
--
-- Number of whole seconds since the Unix epoch. For times before
-- the Unix epoch, this is a negative number. Note that in @%s.%q@ and @%s%Q@
-- the decimals are positive, not negative. For example, 0.9 seconds
-- before the Unix epoch is formatted as @-1.1@ with @%s%Q@.
epoch :: (IsString m, FormatTime a) => Fmt1 m r a
epoch = fmt1 (fm "%s")

-- * For 'Day' (and 'LocalTime' and 'ZonedTime' and 'UTCTime'):

-- | Same as @%m\/%d\/%y@.
dateSlash :: (IsString m, FormatTime a) => Fmt1 m r a
dateSlash = fmt1 (fm "%D")

-- | Same as @%Y-%m-%d@.
dateDash :: (IsString m, FormatTime a) => Fmt1 m r a
dateDash = fmt1 (fm "%F")

-- | As 'dateFmt' @locale@ (e.g. @%m\/%d\/%y@).
dateSlashL :: (IsString m, FormatTime a) => Fmt1 m r a
dateSlashL = fmt1 (fm "%x")

-- | Year.
year :: (IsString m, FormatTime a) => Fmt1 m r a
year = fmt1 (fm "%Y")

-- | Last two digits of year, @00@ - @99@.
yy :: (IsString m, FormatTime a) => Fmt1 m r a
yy = fmt1 (fm "%y")

-- | Century (being the first two digits of the year), @00@ - @99@.
century :: (IsString m, FormatTime a) => Fmt1 m r a
century = fmt1 (fm "%C")

-- | Month name, long form ('fst' from 'months' @locale@), @January@ -
-- @December@.
monthName :: (IsString m, FormatTime a) => Fmt1 m r a
monthName = fmt1 (fm "%B")

-- | @ %H] month name, short form ('snd' from 'months' @locale@),
-- @Jan@ - @Dec@.
monthNameShort :: (IsString m, FormatTime a) => Fmt1 m r a
monthNameShort = fmt1 (fm "%b")

-- | Month of year, leading 0 as needed, @01@ - @12@.
month :: (IsString m, FormatTime a) => Fmt1 m r a
month = fmt1 (fm "%m")

-- | Day of month, leading 0 as needed, @01@ - @31@.
dayOfMonth :: (IsString m, FormatTime a) => Fmt1 m r a
dayOfMonth = fmt1 (fm "%d")

-- | Day of month, @1st@, @2nd@, @25th@, etc.
dayOfMonthOrd :: (IsString m, Semigroup m, FormatTime a) => Fmt1 m r a
dayOfMonthOrd = fmt1 (runFmt ords . toInt)
  where toInt :: FormatTime a => a -> Int
        toInt = read . formatTime defaultTimeLocale "%d"

-- | Day of month, leading space as needed, @ 1@ - @31@.
dayOfMonthS :: (IsString m, FormatTime a) => Fmt1 m r a
dayOfMonthS = fmt1 (fm "%e")

-- | Day of year for Ordinal Date format, @001@ - @366@.
day :: (IsString m, FormatTime a) => Fmt1 m r a
day = fmt1 (fm "%j")

-- | Year for Week Date format e.g. @2013@.
weekYear :: (IsString m, FormatTime a) => Fmt1 m r a
weekYear = fmt1 (fm "%G")

-- | Last two digits of year for Week Date format, @00@ - @99@.
weekYY :: (IsString m, FormatTime a) => Fmt1 m r a
weekYY = fmt1 (fm "%g")

-- | Century (first two digits of year) for Week Date format, @00@ -
-- @99@.
weekCentury :: (IsString m, FormatTime a) => Fmt1 m r a
weekCentury = fmt1 (fm "%f")

-- | Week for Week Date format, @01@ - @53@.
week :: (IsString m, FormatTime a) => Fmt1 m r a
week = fmt1 (fm "%V")

-- | Day for Week Date format, @1@ - @7@.
dayOfWeek :: (IsString m, FormatTime a) => Fmt1 m r a
dayOfWeek = fmt1 (fm "%u")

-- | Day of week, short form ('snd' from 'wDays' @locale@), @Sun@ -
-- @Sat@.
dayNameShort :: (IsString m, FormatTime a) => Fmt1 m r a
dayNameShort = fmt1 (fm "%a")

-- | Day of week, long form ('fst' from 'wDays' @locale@), @Sunday@ -
-- @Saturday@.
dayName :: (IsString m, FormatTime a) => Fmt1 m r a
dayName = fmt1 (fm "%A")

-- | Week number of year, where weeks start on Sunday (as
-- 'sundayStartWeek'), @00@ - @53@.
weekFromZero :: (IsString m, FormatTime a) => Fmt1 m r a
weekFromZero = fmt1 (fm "%U")

-- | Day of week number, @0@ (= Sunday) - @6@ (= Saturday).
dayOfWeekFromZero :: (IsString m, FormatTime a) => Fmt1 m r a
dayOfWeekFromZero = fmt1 (fm "%w")

-- | Week number of year, where weeks start on Monday (as
-- 'mondayStartWeek'), @00@ - @53@.
weekOfYearMon :: (IsString m, FormatTime a) => Fmt1 m r a
weekOfYearMon = fmt1 (fm "%W")

-- * Time spans, diffs, 'NominalDiffTime', 'DiffTime', etc.

-- | Display a time span as one time relative to another. Input is
-- assumed to be seconds. Typical inputs are 'NominalDiffTime' and
-- 'DiffTime'.
diff :: (IsString m, Semigroup m)
     => Bool     -- ^ Display 'in/ago'?
     -> Fmt1 m r Double -- ^ Example: '3 seconds ago', 'in three days'.)
diff fix =
  fmt1 diffed
  where
    diffed ts =
      case find (\(s,_,_) -> abs ts >= s) (reverse ranges) of
        Nothing -> "unkfmtn"
        Just (_,f,base) -> runFmt (prefix % f % suffix) (toInt ts base)
      where prefix = fmt (if fix && ts > 0 then "in " else "")
            suffix = fmt (if fix && ts < 0 then " ago" else "")
    toInt ts base = abs (round (ts / base)) :: Int
    ranges =
      [(0,Fmt.sh % " milliseconds",0.001)
      ,(1,Fmt.sh % " seconds",1)
      ,(minute',const1 "a minute",0)
      ,(minute'*2,Fmt.sh % " minutes",minute')
      ,(minute'*30,const1 "half an hour",0)
      ,(minute'*31,Fmt.sh % " minutes",minute')
      ,(hour',const1 "an hour",0)
      ,(hour'*2,Fmt.sh % " hours",hour')
      ,(hour'*3,const1 "a few hours",0)
      ,(hour'*4,Fmt.sh % " hours",hour')
      ,(day',const1 "a day",0)
      ,(day'*2,Fmt.sh % " days",day')
      ,(week',const1 "a week",0)
      ,(week'*2,Fmt.sh % " weeks",week')
      ,(month',const1 "a month",0)
      ,(month'*2,Fmt.sh % " months",month')
      ,(year',const1 "a year",0)
      ,(year'*2,Fmt.sh % " years",year')]
      where year' = month' * 12
            month' = day' * 30
            week' = day' * 7
            day' = hour' * 24
            hour' = minute' * 60
            minute' = 60

-- | Display the absolute value time span in years.
years :: IsString m => Int -> Fmt1 m r Double
years n = fmt1 (runFmt (Fmt.f n) . abs . count) where count n' = n' / 365 / 24 / 60 / 60

-- | Display the absolute value time span in days.
days :: (IsString m) => Int -> Fmt1 m r Double
days n = fmt1 (runFmt (Fmt.f n) . abs . count) where count n' = n' / 24 / 60 / 60

-- | Display the absolute value time span in hours.
hours :: IsString m => Int -> Fmt1 m r Double
hours n = fmt1 (runFmt (Fmt.f n) . abs . count) where count n' = n' / 60 / 60

-- | Display the absolute value time span in minutes.
minutes :: IsString m => Int -> Fmt1 m r Double
minutes n = fmt1 (runFmt (Fmt.f n) . abs . count) where count n' = n' / 60

-- | Display the absolute value time span in seconds.
seconds :: IsString m => Int -> Fmt1 m r Double
seconds n = fmt1 (runFmt (Fmt.f n) . abs . count) where count n' = n'

{-
-- | Display seconds in the following pattern:
-- @00:00:00:00@, which ranges from days to seconds.
diffComponents :: (RealFloat n) => Fmt1 Builder r n
diffComponents = customDiffComponents (left 2 '0' % ":" % left 2 '0' % ":" % left 2 '0' % ":" % left 2 '0')

-- | Variation of 'diffComponents',
-- which lets you explicitly specify how to render each component.
customDiffComponents :: (RealFloat n) => (forall r'. Fmt Builder r' (Integer -> Integer -> Integer -> Integer -> r')) -> Fmt1 Builder r n
customDiffComponents subFmt = fmt1 builder' where
  builder' diffTime = flip evalState (round diffTime) $ do
    seconds' <- state (swap . flip divMod 60)
    minutes' <- state (swap . flip divMod 60)
    hours' <- state (swap . flip divMod 24)
    days' <- get
    return (runFmt subFmt days' hours' minutes' seconds')
-}
-- * Internal.

-- | Add a suffix to an integral, e.g. 1st, 2nd, 3rd, 21st.
ords :: (IsString m, Semigroup m, Integral n) => Fmt1 m r n
ords = fmt1 go
  where 

    fixed i = runFmt (f i) . realToFrac
    go n
        | tens > 3 && tens < 21 = fixed 0 n <> "th"
        | otherwise =
          fixed 0 n <>
          case n `mod` 10 of
            1 -> "st"
            2 -> "nd"
            3 -> "rd"
            _ -> "th"
        where tens = n `mod` 100


fm :: (FormatTime a, IsString s) => String -> a -> s
fm s = fromString . formatTime defaultTimeLocale s

-- | Helper for creating custom time formatters
customTimeFmt :: (IsString s, FormatTime a) => String -> Fmt1 s r a
customTimeFmt f = fmt1 (fm f)

