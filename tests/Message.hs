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

{- |

Generators and instances for messages and parts thereof.

-}
module Message where

import Data.Char (isAscii, isPrint)
import Data.List.NonEmpty (fromList)

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.MIME

tests :: TestTree
tests = testGroup "message tests"
  [ testProperty "message round trip" prop_messageRoundTrip
  ]

genPrintAsciiChar :: Gen Char
genPrintAsciiChar = arbitraryPrintableChar `suchThat` isAscii

genAscii1 :: Gen T.Text
genAscii1 = T.pack <$> listOf1 genPrintAsciiChar

genText1 :: Gen T.Text
genText1 = T.pack <$> listOf1 arbitraryPrintableChar

genTextPlain :: Gen MIMEMessage
genTextPlain = createTextPlainMessage <$> oneof [genAscii1, genText1]

-- Generate a 50 character multipart boundary.  These MUST be unique
-- for all (nested) multipart messages.  Hopefully 50 chars is
-- enough to have a low-enough probability of collision.
genBoundary :: Gen B.ByteString
genBoundary = B.pack <$> vectorOf 50 genPrintAsciiChar

-- Multipart message with at least one part.
genMultipart1 = depths >>= go
  where
  go :: Int -> Gen MIMEMessage
  go 0 = genTextPlain
  go n = do
    len <- choose (1, 10) -- up to 10 subparts, minimum of 1
    createMultipartMixedMessage
      <$> genBoundary
      <*> ( fromList <$>
            -- 75% plain, 25% nested multipart
            vectorOf len (maybeAp encapsulate 5 $ frequency [(3, genTextPlain), (1, go (n - 1))])
          )

  -- max depth of 4
  depths = frequency
    [ (1, pure 1)
    , (5, pure 2)
    , (3, pure 3)
    , (1, pure 4)
    ]

-- | Apply the function to the generated value with probability 1-in-/n/.
maybeAp :: (a -> a) -> Int -> Gen a -> Gen a
maybeAp f n g = frequency [(n - 1, g), (1, f <$> g)]

genMessage :: Gen MIMEMessage
genMessage = oneof [ genTextPlain, genMultipart1, encapsulate <$> genMessage ]

prop_messageRoundTrip :: Property
prop_messageRoundTrip = forAll genMessage $ \msg ->
  parse (message mime) (renderMessage msg) == Right msg
