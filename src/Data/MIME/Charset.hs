-- This file is part of purebred-email
-- Copyright (C) 2018-2021  Fraser Tweedale
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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |

MIME character sets.

Recognised charsets:

* @us-ascii@ / @iso646-us@
* @utf-8@
* @iso-8859-1@

See also the <https://github.com/purebred-mua/purebred-icu purebred-icu>
plugin, which adds support for many character sets.

-}
module Data.MIME.Charset
  (
    HasCharset(..)
  , CharsetName
  , charsetText
  , charsetText'
  , CharsetError(..)
  , AsCharsetError(..)
  , charsetPrism

  , CharsetLookup
  , defaultCharsets

  , decodeLenient
  ) where

import Control.Lens
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

  -- | Get the declared (or default) character set name.  There is no guarantee
  -- that it corresponds to a registered or supported charset.
  charsetName :: Getter a (Maybe CharsetName)

  -- | Return the encoded data in the structure
  charsetData :: Getter a B.ByteString

  -- | Structure with the encoded data replaced with 'Text'
  charsetDecoded
    :: AsCharsetError e
    => CharsetLookup
    -> ( forall p f. (Profunctor p, Contravariant f)
        => Optic' p f a (Either e (Decoded a)) )

  -- | Structure with the encoded data replaced with 'Text' (monomorphic error type)
  charsetDecoded'
    :: CharsetLookup
    -> ( forall p f. (Profunctor p, Contravariant f)
        => Optic' p f a (Either CharsetError (Decoded a)) )
  charsetDecoded' = charsetDecoded

  -- | Encode the data
  charsetEncode :: Decoded a -> a


-- | Decode the object according to the declared charset.
charsetText
  :: (HasCharset a, AsCharsetError e, Profunctor p, Contravariant f)
  => CharsetLookup -> Optic' p f a (Either e T.Text)
charsetText lookupCharset = to $ \a ->
  maybe (Left $ review _CharsetUnspecified ()) Right (view charsetName a)
  >>= \k -> maybe (Left $ review _CharsetUnsupported k) Right (lookupCharset k)
  >>= \f -> pure (f (view charsetData a))

-- | Monomorphic in error type
charsetText'
  :: (HasCharset a, Profunctor p, Contravariant f)
  => CharsetLookup
  -> Optic' p f a (Either CharsetError T.Text)
charsetText' = charsetText

-- | Prism for charset decoded/encoded data.
-- Information about decoding failures is discarded.
charsetPrism :: forall a. (HasCharset a) => CharsetLookup -> Prism' a (Decoded a)
charsetPrism m = prism' charsetEncode (either err Just . view (charsetDecoded m))
  where
  err = const Nothing :: CharsetError -> Maybe x

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


type CharsetLookup = CI.CI B.ByteString -> Maybe Charset

-- | Supports US-ASCII, UTF-8 and ISO-8859-1.
--
defaultCharsets :: CharsetLookup
defaultCharsets k = lookup k charsets

-- | Decode as UTF-8, replacing invalid sequences with placeholders.
--
decodeLenient :: B.ByteString -> T.Text
decodeLenient = T.decodeUtf8With T.lenientDecode
