{-# LANGUAGE LambdaCase #-}
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
  , TransferEncodingError(..)
  , AsTransferEncodingError(..)
  , TransferEncoding
  , chooseTransferEncoding
  ) where

import Data.Monoid (Sum(Sum), Any(Any))

import Control.Lens
  ( APrism', Getter, Prism'
  , clonePrism, foldMapOf, preview, prism', review, to, view )
import qualified Data.ByteString as B
import Data.ByteString.Lens (bytes)
import qualified Data.CaseInsensitive as CI

import Data.MIME.Base64
import Data.MIME.QuotedPrintable

type TransferEncodingName = CI.CI B.ByteString
type TransferEncoding = APrism' B.ByteString B.ByteString

data TransferEncodingError
  = TransferEncodingUnsupported TransferEncodingName
  | TransferDecodeError TransferEncodingName
  deriving (Show)

class AsTransferEncodingError s where
  _TransferEncodingError :: Prism' s TransferEncodingError
  _TransferEncodingUnsupported :: Prism' s TransferEncodingName
  _TransferDecodeError :: Prism' s TransferEncodingName

  _TransferEncodingUnsupported = _TransferEncodingError . _TransferEncodingUnsupported
  _TransferDecodeError = _TransferEncodingError . _TransferDecodeError

instance AsTransferEncodingError TransferEncodingError where
  _TransferEncodingError = id
  _TransferEncodingUnsupported = prism' TransferEncodingUnsupported $ \case
      TransferEncodingUnsupported k -> Just k ; _ -> Nothing
  _TransferDecodeError = prism' TransferDecodeError $ \case
      TransferDecodeError k -> Just k ; _ -> Nothing


-- | Data types that can have /transfer encoding/.
class HasTransferEncoding a where
  type TransferDecoded a

  -- | Get the declared or default transfer encoding name.
  transferEncodingName :: Getter a TransferEncodingName

  -- | Return the encoded data in the structure.
  transferEncodedData :: Getter a B.ByteString

  -- | Perform content transfer decoding.
  transferDecoded :: AsTransferEncodingError e => Getter a (Either e (TransferDecoded a))

  -- | Perform content transfer decoding (monomorphic error type).
  transferDecoded' :: Getter a (Either TransferEncodingError (TransferDecoded a))
  transferDecoded' = transferDecoded

  -- | Encode the data
  transferEncode :: TransferDecoded a -> a

-- | Decode the object according to the declared content transfer encoding.
transferDecodedBytes
  :: (HasTransferEncoding a, AsTransferEncodingError e)
  => Getter a (Either e B.ByteString)
transferDecodedBytes = to $ \a -> do
  let encName = view transferEncodingName a
  enc <- maybe (Left $ review _TransferEncodingUnsupported encName) Right
    (lookup encName transferEncodings)
  let s = view transferEncodedData a
  maybe (Left $ review _TransferDecodeError encName) Right (preview (clonePrism enc) s)

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

-- | Inspect the data and choose a transfer encoding to use: @7bit@
-- if the data can be transmitted as-is, otherwise whichever of
-- @quoted-printable@ or @base64@ should result in smaller output.
--
chooseTransferEncoding :: B.ByteString -> (TransferEncodingName, TransferEncoding)
chooseTransferEncoding s
  -- TODO: does not handle max line length of 998
  | not doEnc = ("7bit", id)
  | nQP < nB64 = ("quoted-printable", contentTransferEncodingQuotedPrintable)
  | otherwise = ("base64", contentTransferEncodingBase64)
  where
    -- https://tools.ietf.org/html/rfc5322#section-3.5 'text'
    needEnc c = c > 127 || c == 0
    qpBytes c
      | encodingRequiredNonEOL QuotedPrintable c = 3
      | otherwise = 1
    (Any doEnc, Sum nQP) = foldMapOf bytes (\c -> (Any (needEnc c), Sum (qpBytes c))) s
    nB64 = ((B.length s + 2) `div` 3) * 4
