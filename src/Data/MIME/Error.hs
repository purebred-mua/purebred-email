{-# LANGUAGE LambdaCase #-}

module Data.MIME.Error where

import Control.Lens (Prism', prism')

import Data.MIME.Charset
import Data.MIME.TransferEncoding


-- | Transfer or character encoding errors
--
data EncodingError
  = TransferEncodingError TransferEncodingError
  | CharsetError CharsetError
  deriving (Show)

class AsEncodingError s where
  _EncodingError :: Prism' s EncodingError
  _EncodingErrorTransferEncodingError :: Prism' s TransferEncodingError
  _EncodingErrorCharsetError :: Prism' s CharsetError

  _EncodingErrorTransferEncodingError = _EncodingError . _TransferEncodingError
  _EncodingErrorCharsetError = _EncodingError . _CharsetError

instance AsEncodingError EncodingError where
  _EncodingError = id
  _EncodingErrorTransferEncodingError = prism' TransferEncodingError $ \case
    TransferEncodingError e -> Just e ; _ -> Nothing
  _EncodingErrorCharsetError = prism' CharsetError $ \case
    CharsetError e -> Just e ; _ -> Nothing

instance AsCharsetError EncodingError where
  _CharsetError = _EncodingErrorCharsetError

instance AsTransferEncodingError EncodingError where
  _TransferEncodingError = _EncodingErrorTransferEncodingError
