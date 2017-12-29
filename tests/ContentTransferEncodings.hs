{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module ContentTransferEncodings where

import Control.Lens (preview, review)
import qualified Data.ByteString as B

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances ()

import Data.MIME.Base64 (contentTransferEncodingBase64)
import Data.MIME.QuotedPrintable (contentTransferEncodingQuotedPrintable)
import Data.MIME.Types (ContentTransferEncoding)


properties :: TestTree
properties =
  localOption (QuickCheckMaxSize 1000) $
  testGroup "Content-Transfer-Encoding properties"
    [ testProperty "base64 round-trip"
        (prop_roundtrip contentTransferEncodingBase64)
    , testProperty "quoted-printable round-trip"
        (prop_roundtrip contentTransferEncodingQuotedPrintable)
    , testProperty "base64 line length <= 76"
        (prop_linelength contentTransferEncodingBase64)
    , testProperty "quoted-printable line length <= 76"
        (prop_linelength contentTransferEncodingQuotedPrintable)
    ]

prop_roundtrip :: ContentTransferEncoding -> B.ByteString -> Bool
prop_roundtrip p s = preview p (review p s) == Just s

prop_linelength :: ContentTransferEncoding -> B.ByteString -> Property
prop_linelength p s =
  let
    encoded = review p s
    prop = all ((<= 76) . B.length) (splitOnCRLF encoded)
  in
    cover (B.length encoded > 100) 50 "long output" prop

splitOnCRLF :: B.ByteString -> [B.ByteString]
splitOnCRLF s =
  let (l, r) = B.breakSubstring "\r\n" s
  in
    if B.null r
      then [l]
      else l : splitOnCRLF (B.drop 2 r)
