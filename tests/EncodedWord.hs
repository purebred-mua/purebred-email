module EncodedWord where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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
