-- This file is part of purebred-email
-- Copyright (C) 2019, 2021  Fraser Tweedale
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

module Parser where

import Data.Either (isLeft)

import Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as L
import Data.Time (ZonedTime(ZonedTime), timeZoneMinutes)
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.RFC5322
import Data.RFC5322.Internal (takeTillString)

tests :: TestTree
tests = testGroup "Parser tests"
  [ testProperty "takeTillString" $ \h l c r ->
      let
        s = h <> l <> c <> r
        s' = L.fromChunks $ B.foldr ((:) . B.singleton) [] s
      in
        not (c `B.isInfixOf` l) ==>
          case AL.parse (AL.string h *> takeTillString c) s' of
            AL.Done r' l' -> r' == L.fromStrict r && l' == l
            _ -> False
  , testBodyParsing
  , testDateTimeParsing
  ]


data Antibody = Antibody
  deriving (Eq, Show)

instance EqMessage Antibody

data MaybeBody = NotPresent | Present B.ByteString
  deriving (Eq, Show)

instance EqMessage MaybeBody


testBodyParsing :: TestTree
testBodyParsing = testGroup "BodyHandler tests"
  [ testCase "empty body" $
      parseOnly (message (const $ NoBody Antibody) <* endOfInput) msg
      @?= Right (Message hdrs Antibody)
  , testCase "optional body (present, parses)" $
      parseOnly (message opt <* endOfInput) (msg <> "\r\nyeah")
      @?= Right (Message hdrs (Present "yeah"))
  , testCase "optional body (present, no parse)" $
      isLeft (parseOnly (message opt <* endOfInput) (msg <> "\r\nnah"))
      @?= True
  , testCase "optional body (present, empty)" $
      isLeft (parseOnly (message opt <* endOfInput) (msg <> "\r\n"))
      @?= True
  , testCase "optional body (absent)" $
      parseOnly (message opt <* endOfInput) msg
      @?= Right (Message hdrs NotPresent)
  ]
  where
    msg = "From: alice@example.net\r\n"
    hdrs = Headers [("From", "alice@example.net")]
    opt _ = OptionalBody (Present <$> AL.string "yeah", NotPresent)

testDateTimeParsing :: TestTree
testDateTimeParsing = testGroup "dateTime parsing" $
  let
    now = explode $ read "2021-01-03 20:16:00 +1000"
    go = fmap explode . parseOnly (dateTime <* endOfInput)
    explode (ZonedTime lt z) = (lt, timeZoneMinutes z)
  in
    [ testCase "good (full)" $
        go "Sun, 03 Jan 2021 20:16:00 +1000"
        @?= Right now
    , testCase "good (comment)" $
        go "Sun, 03 Jan 2021 20:16:00 +1000 (wat)"
        @?= Right now
    , testCase "good (extra whitespace)" $
        go "  Sun,  03  Jan  2021  20:16:00  +1000  "
        @?= Right now
    , testCase "good (no seconds)" $
        go "Sun, 03 Jan 2021 20:16 +1000"
        @?= Right now
    , testCase "good (no day-of-week)" $
        go "03 Jan 2021 20:16:00 +1000"
        @?= Right now
    , testCase "good (single-digit date)" $
        go "Sun, 3 Jan 2021 20:16:00 +1000"
        @?= Right now
    , testCase "good (leap second)" $
        go "Sun, 03 Jan 2021 20:16:60 +1000"
        @?= Right (explode $ read "2021-01-03 20:16:60 +1000")
    , testCase "good (5-digit year)" $
        go "Sun, 03 Jan 12021 20:16:00 +1000"
        @?= Right (explode $ read "12021-01-03 20:16:00 +1000")
    , testCase "bad (invalid day-of-week)" $
        isLeft (go "Moo, 03 Jan 2021 20:16:00 +1000")
        @?= True
    , testCase "bad (inconsistent day-of-week)" $
        isLeft (go "Mon, 03 Jan 2021 20:16:00 +1000")
        @?= True
    , testCase "bad (day < 1)" $
        isLeft (go "Thu, 01 Jan 2021 20:16:00 +1000")
        @?= True
    , testCase "bad (out of range day)" $
        isLeft (go "Tue, 33 Jan 2021 20:16:00 +1000")
        @?= True
    , testCase "bad (inconsistent day of month)" $
        isLeft (go "Mon, 29 Feb 2021 20:16:00 +1000")
        @?= True
    , testCase "bad (invalid month)" $
        isLeft (go "Sun, 03 Joo 2021 20:16:00 +1000")
        @?= True
    , testCase "bad (year insufficient digits)" $
        isLeft (go "03 Jan 9 20:16:00 +1000")
        @?= True
    , testCase "bad (year < 1900)" $
        isLeft (go "Sun, 03 Jan 1899 20:16:00 +1000")
        @?= True
    , testCase "bad (hour > 23)" $
        isLeft (go "Sun, 03 Jan 2021 24:16:00 +1000")
        @?= True
    , testCase "bad (minute > 59)" $
        isLeft (go "Sun, 03 Jan 2021 20:60:00 +1000")
        @?= True
    , testCase "bad (second > 60)" $
        isLeft (go "Sun, 03 Jan 2021 20:16:61 +1000")
        @?= True
    , testCase "bad (no timezone)" $
        isLeft (go "Sun, 03 Jan 2021 20:16:00")
        @?= True
    , testCase "bad (invalid tz seconds)" $
        isLeft (go "Sun, 03 Jan 2021 20:16:00 +0960")
        @?= True
    , testCase "good (obs-zone grammar)" $
        (fmap (fmap snd) . traverse go . fmap ("Sun, 03 Jan 2021 20:16:00 " <>))
          (
            ["UT", "GMT", "EST", "EDT", "CST", "CDT", "MST", "MDT", "PST", "PDT"]
            <> fmap Char8.singleton (['A'..'I'] <> ['K'..'Z'] <> ['a'..'i'] <> ['k'..'z'])
          )
        @?= Right (
          let mil = [1..12] <> fmap negate [1..12] <> [0]
          in fmap (* 60) ([0, 0, -5, -4, -6, -5, -7, -6, -8, -7] <> mil <> mil)
        )
    , testCase "bad (obs-zone grammar)" $
        all (isLeft . go) (("Sun, 03 Jan 2021 20:16:00 " <>) <$> ["UTC", "CET", "AEST", "J"])
        @?= True
    , testCase "good (obs-year 2 digit)" $
        go "Sun, 03 Jan 21 20:16:00 +1000"
        @?= Right now
    , testCase "good (obs-year 3 digit)" $
        go "Sun, 03 Jan 121 20:16:00 +1000"
        @?= Right now
    , testCase "good (obs-year 2 digit 2049)" $
        go "Sun, 03 Jan 49 20:16:60 +1000"
        @?= Right (explode $ read "2049-01-03 20:16:60 +1000")
    , testCase "good (obs-year 2 digit 1950)" $
        go "Tue, 03 Jan 50 20:16:60 +1000"
        @?= Right (explode $ read "1950-01-03 20:16:60 +1000")
    ]
