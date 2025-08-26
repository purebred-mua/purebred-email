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

{-# LANGUAGE OverloadedStrings #-}
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

  -- us-ascii aliases
  , ("ISO646-US", us_ascii)
  , ("ANSI_X3.4-1968", us_ascii)
  , ("iso-ir-6", us_ascii)
  , ("ANSI_X3.4-1986", us_ascii)
  , ("ISO_646.irv:1991", us_ascii)
  , ("us", us_ascii)
  , ("IBM367", us_ascii)
  , ("cp367", us_ascii)
  , ("csASCII.4-1968", us_ascii)
  , ("ascii", us_ascii)  -- https://github.com/purebred-mua/purebred-email/issues/69

  -- iso-8859-1 aliases
  , ("iso-ir-100", iso_8859_1)
  , ("ISO_8859-1", iso_8859_1)
  , ("latin1", iso_8859_1)
  , ("l1", iso_8859_1)
  , ("IBM819", iso_8859_1)
  , ("CP819", iso_8859_1)
  , ("csISOLatin1", iso_8859_1)

  -- utf-8 aliases
  , ("csUTF8", utf_8)

  -- utf-16
  , ("UTF-16BE", utf16be)
  , ("UTF-16LE", utf16le)
  , ("UTF-16", utf16)
  , ("csUTF16BE", utf16be)
  , ("csUTF16LE", utf16le)
  , ("csUTF16", utf16)

  -- utf-32
  , ("UTF-32BE", utf32be)
  , ("UTF-32LE", utf32le)
  , ("UTF-32", utf32)
  , ("csUTF32BE", utf32be)
  , ("csUTF32LE", utf32le)
  , ("csUTF32", utf32)

  -- Other charsets I observed in my mail corpus.
  -- , ("iso-8859-2", ...)
  -- , ("iso-8859-15", ...)
  -- , ("iso-2022-jp", ...)    (common)
  -- , ("windows-1252", ...)   (common)
  -- , ("windows-1256", ...)
  -- , ("cp1252", ...)         (alias of windows-1252)
  -- , ("big5", ...)           (common)
  -- , ("euc-kr", ...)
  -- , ("cp932", ...)
  -- , ("gb2312", ...)         (Chinese)
  ]

us_ascii, utf_8, iso_8859_1 :: Charset
us_ascii = decodeLenient
utf_8 = decodeLenient
iso_8859_1 = T.decodeLatin1

utf16be, utf16le, utf16 :: Charset
utf16be = T.decodeUtf16BEWith T.lenientDecode
utf16le = T.decodeUtf16LEWith T.lenientDecode
-- https://www.rfc-editor.org/rfc/rfc2781.html#section-4.3
utf16 b = case B.take 2 b of
  "\xff\xfe"  -> utf16le b
  _           -> utf16be b

utf32be, utf32le, utf32 :: Charset
utf32be = T.decodeUtf32BEWith T.lenientDecode
utf32le = T.decodeUtf32LEWith T.lenientDecode
-- Unicode 4.0, Section 3.10, D45
-- https://www.unicode.org/versions/Unicode4.0.0/ch03.pdf#G7404
utf32 b = case B.take 4 b of
  "\xff\xfe\x00\x00"  -> utf32le b
  _                   -> utf32be b


type CharsetLookup = CI.CI B.ByteString -> Maybe Charset

-- | Supports US-ASCII, UTF-8 and ISO-8859-1, UTF-16[BE|LE]
-- and UTF-32[BE|LE].  The /purebred-icu/ package provides
-- support for more charsets.
--
defaultCharsets :: CharsetLookup
defaultCharsets k = lookup k charsets

-- | Decode as UTF-8, replacing invalid sequences with placeholders.
--
decodeLenient :: B.ByteString -> T.Text
decodeLenient = T.decodeUtf8With T.lenientDecode
