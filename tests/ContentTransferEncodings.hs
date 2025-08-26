-- This file is part of purebred-email
-- Copyright (C) 2017-2018  Róman Joost and Fraser Tweedale
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

module ContentTransferEncodings where

import Data.Word

import Control.Lens (clonePrism, preview, review)
import qualified Data.ByteString as B

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances ()

import Data.MIME.Base64 (contentTransferEncodingBase64)
import Data.MIME.QuotedPrintable
import Data.MIME.Types


properties :: TestTree
properties = localOption (QuickCheckMaxSize 1000) $
  testGroup "codec properties"
    [ testGroup "Content-Transfer-Encoding properties"
      [ testProperty "base64 round-trip"
          (prop_roundtrip contentTransferEncodingBase64)
      , testProperty "quoted-printable round-trip"
          (prop_roundtrip contentTransferEncodingQuotedPrintable)
      , testProperty "base64 line length <= 76"
          (prop_linelength contentTransferEncodingBase64)
      , testProperty "quoted-printable line length <= 76"
          (prop_linelength contentTransferEncodingQuotedPrintable)
      ]
    , testGroup "encoded-word codec properties"
      [ testProperty "Q round-trip" (prop_roundtrip q)
      , testProperty "Q does not contain spaces" (prop_notElem q 32 {- ' ' -})
      ]
    ]


prop_roundtrip :: ContentTransferEncoding -> B.ByteString -> Bool
prop_roundtrip p s = preview (clonePrism p) (review (clonePrism p) s) == Just s

prop_linelength :: ContentTransferEncoding -> B.ByteString -> Property
prop_linelength p s =
  let
    encoded = review (clonePrism p) s
    prop = all ((<= 76) . B.length) (splitOnCRLF encoded)
  in
    checkCoverage $ cover 50 (B.length encoded > 100) "long output" prop

prop_notElem :: EncodedWordEncoding -> Word8 -> B.ByteString -> Bool
prop_notElem enc c = B.notElem c . review (clonePrism enc)

splitOnCRLF :: B.ByteString -> [B.ByteString]
splitOnCRLF s =
  let (l, r) = B.breakSubstring "\r\n" s
  in
    if B.null r
      then [l]
      else l : splitOnCRLF (B.drop 2 r)
