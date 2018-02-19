{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |

MIME character sets.

Recognised charsets:

* @us-ascii@ / @iso646-us@
* @utf-8@
* @iso-8859-1@

-}
module Data.MIME.Charset
  (
    HasCharset(..)
  , CharsetName
  , charsetText
  , charsetText'
  , CharsetError(..)
  , AsCharsetError(..)

  , decodeLenient
  ) where

import Control.Lens (Getter, Prism', prism', review, to, view)
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

type CharsetName = CI.CI B.ByteString
type Charset = B.ByteString -> T.Text  -- eventually we might want a prism

data CharsetError
  = CharsetUnspecified
  | CharsetUnsupported CharsetName
  | CharsetDecodeError CharsetName
  deriving (Show)

class AsCharsetError s where
  _CharsetError :: Prism' s CharsetError
  _CharsetUnspecified :: Prism' s ()
  _CharsetUnsupported :: Prism' s CharsetName
  _CharsetDecodeError :: Prism' s CharsetName

  _CharsetUnspecified = _CharsetError . _CharsetUnspecified
  _CharsetUnsupported = _CharsetError . _CharsetUnsupported
  _CharsetDecodeError = _CharsetError . _CharsetDecodeError

instance AsCharsetError CharsetError where
  _CharsetError = id
  _CharsetUnspecified = prism' (const CharsetUnspecified) $ \case
      CharsetUnspecified -> Just () ; _ -> Nothing
  _CharsetUnsupported = prism' CharsetUnsupported $ \case
      CharsetUnsupported k -> Just k ; _ -> Nothing
  _CharsetDecodeError = prism' CharsetDecodeError $ \case
      CharsetDecodeError k -> Just k ; _ -> Nothing


class HasCharset a where
  type Decoded a

  -- | Get the declared character set name.  There is no guarantee
  -- that it corresponds to a registered or supported charset.
  charsetName :: Getter a (Maybe CharsetName)

  -- | Return the encoded data in the structure
  charsetData :: Getter a B.ByteString

  -- | Structure with the encoded data replaced with 'Text'
  charsetDecoded :: AsCharsetError e => Getter a (Either e (Decoded a))


-- | Decode the object according to the declared charset.
charsetText :: (HasCharset a, AsCharsetError e) => Getter a (Either e T.Text)
charsetText = to $ \a ->
  maybe (Left $ review _CharsetUnspecified ()) Right (view charsetName a)
  >>= \k -> maybe (Left $ review _CharsetUnsupported k) Right (lookupCharset k)
  >>= \f -> pure (f (view charsetData a))

-- | Monomorphic in error type
charsetText' :: (HasCharset a) => Getter a (Either CharsetError T.Text)
charsetText' = charsetText

charsets :: [(CI.CI B.ByteString, Charset)]
charsets =
  [ ("us-ascii", us_ascii)
  , ("utf-8", utf_8)
  , ("iso-8859-1", iso_8859_1)

  -- uncommon aliases
  , ("ISO646-US", us_ascii)
  , ("ANSI_X3.4-1968", us_ascii)

  -- , ("iso-8859-2", ...)
  -- , ("iso-8859-15", ...)
  -- , ("iso-2022-jp", ...)    (common)
  -- , ("windows-1252", ...)   (common)
  -- , ("windows-1256", ...)
  -- , ("cp1252", ...)         (same as windows-1256?)
  -- , ("big5", ...)           (common)
  -- , ("euc-kr", ...)
  -- , ("cp932", ...)
  -- , ("gb2312", ...)         (Chinese)
  ]

us_ascii, utf_8, iso_8859_1 :: Charset
us_ascii = decodeLenient
utf_8 = decodeLenient
iso_8859_1 = T.decodeLatin1

-- | Look up a character set.
--
lookupCharset :: CI.CI B.ByteString -> Maybe Charset
lookupCharset k = lookup k charsets

-- | Decode as UTF-8, replacing invalid sequences with placeholders.
--
decodeLenient :: B.ByteString -> T.Text
decodeLenient = T.decodeUtf8With T.lenientDecode
