-- This file is part of purebred-email
-- Copyright (C) 2020  Stephen Paul Weber and Fraser Tweedale
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

module EncodedWord where

import Data.MIME.Charset
import Data.MIME.EncodedWord

import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

properties :: TestTree
properties = localOption (HedgehogTestLimit (Just 1000)) $
  testProperty "encodeEncodedWords roundtrip" prop_roundtrip

prop_roundtrip :: Property
prop_roundtrip = property $ do
  t <- forAll $ Gen.text (Range.linear 0 1000) Gen.unicode
  decodeEncodedWords defaultCharsets (encodeEncodedWords t) === t
