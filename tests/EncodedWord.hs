module EncodedWord where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances ()
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.MIME.Charset
import Data.MIME.EncodedWord

properties :: TestTree
properties = localOption (QuickCheckMaxSize 1000) $
  testProperty "encodeEncodedWords roundtrip" prop_roundtrip

prop_roundtrip :: T.Text -> Bool
prop_roundtrip t =
  decodeEncodedWords defaultCharsets (encodeEncodedWords t) == t
