-- This file is part of purebred-email
-- Copyright (C) 2021  Fraser Tweedale
--
-- purebred-email is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.DateTime
  ( dateTime
  ) where

import Control.Applicative ((<|>), optional)
import Control.Monad (guard)
import Data.Functor (($>))

import Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString.Char8 (char8, isDigit_w8)
import qualified Data.ByteString as B
import qualified Data.Time
import Data.Time
  ( Day, DayOfWeek(..), LocalTime(LocalTime), TimeOfDay, TimeZone(TimeZone)
  , ZonedTime(ZonedTime), fromGregorianValid, makeTimeOfDayValid
  , minutesToTimeZone, hoursToTimeZone, utc
  )
import Data.IMF.Internal (fws, optionalCFWS, optionalFWS)

dateTime :: Parser ZonedTime
dateTime = do
  dow <- optional (dayOfWeek <* char8 ',')
  theDate <- date

  -- ensure day of week matches date
  case dow of
    Just dow' | Data.Time.dayOfWeek theDate /= dow'
      -> fail "day of week inconsistent with date"
    _ -> pure ()

  tod <- timeOfDay
  z <- zone
  _ <- optionalCFWS

  pure $ ZonedTime (LocalTime theDate tod) z


dayOfWeek :: Parser DayOfWeek
dayOfWeek = optionalFWS *> dayName

dayName :: Parser DayOfWeek
dayName =
  string "Mon" $> Monday
  <|> string "Tue" $> Tuesday
  <|> string "Wed" $> Wednesday
  <|> string "Thu" $> Thursday
  <|> string "Fri" $> Friday
  <|> string "Sat" $> Saturday
  <|> string "Sun" $> Sunday
  <|> fail "invalid day-of-week"

date :: Parser Day
date = do
  d <- day
  m <- month
  y <- year
  case fromGregorianValid y m d of
    Just r -> pure r
    Nothing -> fail "invalid date"

day :: Parser Int
day = optionalFWS *> go <* fws
  where
  go = (twoDigit <|> digit) >>= check (\n -> n > 0 && n <= 31) "day out of range"

month :: Parser Int
month =
  string "Jan" $> 1
  <|> string "Feb" $> 2
  <|> string "Mar" $> 3
  <|> string "Apr" $> 4
  <|> string "May" $> 5
  <|> string "Jun" $> 6
  <|> string "Jul" $> 7
  <|> string "Aug" $> 8
  <|> string "Sep" $> 9
  <|> string "Oct" $> 10
  <|> string "Nov" $> 11
  <|> string "Dec" $> 12
  <|> fail "invalid month"

year :: Parser Integer
year = fws *> (go >>= check (>= 1900) "year cannot be < 1900") <* fws
  where
  go = fourOrMoreDigit <|> obsYear <|> fail "too few digits in year"
  fourOrMoreDigit = do
    digits <- A.takeWhile isDigit_w8
    guard (B.length digits >= 4)
    pure (B.foldl' step 0 digits)
  step r a = r * 10 + fromIntegral (a - 48)
  obsYear = do
    yy <- twoDigit
    fromIntegral
      . maybe (yy + if yy <= 49 then 2000 else 1900) (1900 + yy * 10 +)
      <$> optional digit

timeOfDay :: Parser TimeOfDay
timeOfDay = do
  hour <- twoDigit
  _ <- char8 ':'
  minute <- twoDigit
  second <- char8 ':' *> twoDigit <|> pure 0
  case makeTimeOfDayValid hour minute (fromIntegral second) of
    Nothing -> fail "invalid time-of-day"
    Just tod -> pure tod

zone :: Parser TimeZone
zone = fws *> (go <|> obsZone)
  where
  go = do
    posNeg <- char8 '+' $> id <|> char8 '-' $> negate
    h <- twoDigit
    m <- twoDigit
    guardFail (m <= 59) "zone minutes must be in range 0..59"
    pure $ minutesToTimeZone (posNeg (h * 60 + m))

obsZone :: Parser TimeZone
obsZone =
  utc <$ (string "GMT" <|> string "UT")
  <|> usZone
  <|> milZone
  where
  usZone = do
    (off, c1) <-
      charVal (-5) 'E'      -- eastern
      <|> charVal (-6) 'C'  -- central
      <|> charVal (-7) 'M'  -- mountain
      <|> charVal (-8) 'P'  -- pacific
    (dst, c2) <- charVal 0 'S' <|> charVal 1 'D'  -- standard / dst
    _ <- char8 'T'
    pure $ TimeZone ((off + dst) * 60) (dst == 1) (c1:c2:"T")
  charVal a c = (a, c) <$ char8 c
  milZone =
    utc <$ (char8 'Z' <|> char8 'z')
    <|> go     id 0x40 0x41 0x49  -- A..I
    <|> go     id 0x41 0x4b 0x4d  -- K..M
    <|> go negate 0x4d 0x4c 0x59  -- N..Y
    <|> go     id 0x60 0x61 0x69  -- a..i
    <|> go     id 0x61 0x6b 0x6d  -- k..m
    <|> go negate 0x6d 0x6e 0x79  -- n..y
  go f off lo hi =
    hoursToTimeZone . f . subtract off . fromIntegral
    <$> satisfy (\c -> c >= lo && c <= hi)


guardFail :: Bool -> String -> Parser ()
guardFail True _ = pure ()
guardFail False s = fail s

check :: (a -> Bool) -> String -> a -> Parser a
check f s a = guardFail (f a) s $> a

digit :: Parser Int
digit = (\c -> fromIntegral (c - 48)) <$> satisfy isDigit_w8

twoDigit :: Parser Int
twoDigit = (\hi lo -> hi * 10 + lo) <$> digit <*> digit
