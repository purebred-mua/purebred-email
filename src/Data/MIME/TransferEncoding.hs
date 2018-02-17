{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |

MIME content transfer encodings.

-}
module Data.MIME.TransferEncoding
  (
    HasTransferEncoding(..)
  , TransferEncodingName
  , transferDecodedBytes
  , transferEncodings
  ) where

import Control.Lens (APrism', Getter, clonePrism, preview, to, view)
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI

import Data.MIME.Base64
import Data.MIME.QuotedPrintable

type TransferEncodingName = CI.CI B.ByteString
type TransferEncoding = APrism' B.ByteString B.ByteString

data TransferEncodingError
  = TransferEncodingUnsupported TransferEncodingName
  | TransferDecodeError TransferEncodingName B.ByteString
  deriving (Show)

class HasTransferEncoding a where
  type TransferDecoded a

  -- | Get the declared or default transfer encoding name.
  transferEncodingName :: Getter a TransferEncodingName

  -- | Return the encoded data in the structure
  transferEncodedData :: Getter a B.ByteString

  -- | Structure with the encoded data replaced with 'Text'
  transferDecoded :: Getter a (Either TransferEncodingError (TransferDecoded a))

-- | Decode the object according to the declared charset.
transferDecodedBytes
  :: HasTransferEncoding a
  => Getter a (Either TransferEncodingError B.ByteString)
transferDecodedBytes = to $ \a -> do
  let encName = view transferEncodingName a
  enc <- maybe (Left $ TransferEncodingUnsupported encName) Right
    (lookup encName transferEncodings)
  let s = view transferEncodedData a
  maybe (Left $ TransferDecodeError encName s) Right (preview (clonePrism enc) s)

-- Message instance:
    --v = fromMaybe "7bit" $ preview (header "content-transfer-encoding") h
-- | Get the Content-Transfer-Encoding for an entity.
-- Defaults to @7bit@ (RFC 2045 §6.1) if the header is
-- not present.  Fails on /unrecognised/ values.
--

transferEncodings :: [(CI.CI B.ByteString, TransferEncoding)]
transferEncodings =
  [ ("7bit", id)
  , ("8bit", id)
  , ("binary", id)
  , ("quoted-printable", contentTransferEncodingQuotedPrintable)
  , ("base64", contentTransferEncodingBase64)
  , ("q", q)
  , ("b", contentTransferEncodingBase64)
  ]
