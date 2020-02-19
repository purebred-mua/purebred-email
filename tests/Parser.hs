-- This file is part of purebred-email
-- Copyright (C) 2019  Fraser Tweedale
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
import Data.ByteString as B
import Data.ByteString.Lazy as L
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
