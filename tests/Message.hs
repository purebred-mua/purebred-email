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

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |

Generators and properties for messages and parts thereof.

-}
module Message where

import Data.Char (isPrint)
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty, intersperse)

import Control.Lens (set, view)
import qualified Data.ByteString as B
import qualified Data.Text as T

import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.MIME
import Data.RFC5322.Internal (isAtext)

tests :: TestTree
tests = testGroup "message tests"
  [ testProperty "message round trip" prop_messageRoundTrip
  , localOption (HedgehogTestLimit (Just 10000)) $
      testProperty "message round trip with From header" prop_messageFromRoundTrip
  ]

printableAsciiChar, printableUnicodeChar, unicodeCharAsciiBias :: Gen Char
printableAsciiChar = Gen.filter isPrint Gen.ascii
printableUnicodeChar = Gen.filter isPrint Gen.unicode

-- | We need a generator that has ascii bias to make sequences
-- like "\r\n" more likely.
unicodeCharAsciiBias = Gen.frequency [(3, Gen.ascii), (1, Gen.unicode)]

asciiText1, unicodeText1 :: Gen T.Text
asciiText1 = Gen.text (Range.linear 1 100) printableAsciiChar
unicodeText1 = Gen.text (Range.linear 1 100) printableUnicodeChar

genTextPlain, genMultipart, genMessage :: Gen MIMEMessage
genTextPlain = createTextPlainMessage <$> Gen.choice [asciiText1, unicodeText1]
genMultipart = depths >>= go
  where
  -- Generate a 50 character multipart boundary.  These MUST be unique
  -- for all (nested) multipart messages.  Assume negligible probability
  -- of collision.
  genBoundary = Gen.utf8 (Range.singleton 50) printableAsciiChar

  go 0 = genTextPlain
  go n = createMultipartMixedMessage
      <$> genBoundary
      <*> ( Gen.nonEmpty (Range.linear 1 10) $
            -- 75% plain, 25% nested multipart
            maybeAp encapsulate 5 $ Gen.frequency [(3, genTextPlain), (1, go (n - 1))]
          )

  -- max depth of 4
  depths :: Gen Int
  depths = Gen.frequency
    [ (1, pure 1)
    , (5, pure 2)
    , (3, pure 3)
    , (1, pure 4)
    ]

  -- Apply the function to the generated value with probability 1-in-/n/.
  maybeAp f n g = Gen.frequency [(n - 1, g), (1, f <$> g)]

genMessage = Gen.choice [ genTextPlain, genMultipart, encapsulate <$> genMessage ]

prop_messageRoundTrip :: Property
prop_messageRoundTrip = property $ do
  msg <- forAll genMessage
  parse (message mime) (renderMessage msg) === Right msg

prop_messageFromRoundTrip :: Property
prop_messageFromRoundTrip = property $ do
  from <- forAll genMailbox
  let
    l = headerFrom defaultCharsets
    msg = set l [from] (createTextPlainMessage "Hello")
  (view l <$> parse (message mime) (renderMessage msg)) === Right [from]

genDomain :: Gen Domain
genDomain = DomainDotAtom <$> genDotAtom -- TODO domain literal

genDotAtom :: Gen (NonEmpty B.ByteString)
genDotAtom = Gen.nonEmpty (Range.linear 1 5) (Gen.utf8 (Range.linear 1 20) (Gen.filter isAtext Gen.ascii))

genLocalPart :: Gen B.ByteString
genLocalPart = fold . intersperse "." <$> genDotAtom

genAddrSpec :: Gen AddrSpec
genAddrSpec = AddrSpec <$> genLocalPart <*> genDomain

genMailbox :: Gen Mailbox
genMailbox =
  Mailbox
  <$> Gen.maybe (Gen.text (Range.linear 0 100) unicodeCharAsciiBias)
  <*> genAddrSpec
