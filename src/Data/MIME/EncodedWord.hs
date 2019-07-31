{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |

@encoded-word@s for representing non-7bit ASCII data in headers
(RFC 2047 and RFC 2231).

-}
module Data.MIME.EncodedWord
  (
    decodeEncodedWords
  ) where

import Control.Applicative ((<|>), liftA2, optional)
import Data.Bifunctor (first)
import Data.Semigroup ((<>))

import Control.Lens (to, clonePrism, review, view)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (char8)
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.MIME.Charset
import Data.MIME.TransferEncoding
import Data.MIME.QuotedPrintable
import Data.RFC5322.Internal

data EncodedWord = EncodedWord
  { _encodedWordCharset :: CI.CI B.ByteString
  , _encodedWordLanguage :: Maybe (CI.CI B.ByteString)
  , _encodedWordEncoding :: CI.CI B.ByteString
  , _encodedWordText :: B.ByteString
  }

instance HasTransferEncoding EncodedWord where
  type TransferDecoded EncodedWord = TransferDecodedEncodedWord
  transferEncodingName = to _encodedWordEncoding
  transferEncodedData = to _encodedWordText
  transferDecoded = to $ \a@(EncodedWord charset lang _ _) ->
    TransferDecodedEncodedWord charset lang <$> view transferDecodedBytes a
  transferEncode = transferEncodeEncodedWord


data TransferDecodedEncodedWord = TransferDecodedEncodedWord
  { _transDecWordCharset :: CI.CI B.ByteString
  , _transDecWordLanguage :: Maybe (CI.CI B.ByteString)
  , _transDecWordText :: B.ByteString
  }

-- | 'charsetEncode' uses UTF-8, but sets the charset to @us-ascii@
-- if the text only contains ASCII characters.  No language is set
-- upon encoding.
--
instance HasCharset TransferDecodedEncodedWord where
  type Decoded TransferDecodedEncodedWord = T.Text
  charsetName = to (pure . _transDecWordCharset)
  charsetData = to _transDecWordText
  charsetDecoded = charsetText

  charsetEncode s =
    let
      bs = T.encodeUtf8 s
      charset = if B.all (< 0x80) bs then "us-ascii" else "utf-8"
    in TransferDecodedEncodedWord charset Nothing bs

-- NOTE: may not be > 75 chars long
--
-- NOTE: DOES NOT PARSE THE LEADING "=?"
--
encodedWord :: Parser EncodedWord
encodedWord =
  EncodedWord
  <$> ci token
  <*> optional (char8 '*' *> ci token)
  <*> (qmark *> ci token)
  <*> (qmark *> encodedText <* qmark <* eq)
  where
    eq = char8 '='
    qmark = char8 '?'
    token = takeWhile1 (\c -> c >= 33 && c <= 126 && notInClass "()<>@,;:\\\"/[]?.=*" c)

    -- any printable ascii char other than ? or SPACE
    encodedText = takeWhile1 (\c -> (c >= 33 && c < 63) || (c > 63 && c <= 127))

serialiseEncodedWord :: EncodedWord -> B.ByteString
serialiseEncodedWord (EncodedWord charset lang enc s) =
  "=?" <> CI.original charset
  <> maybe "" (\l -> "*" <> CI.original l) lang
  <> "?" <> CI.original enc <> 
  "?" <> s <> "?="

transferEncodeEncodedWord :: TransferDecodedEncodedWord -> EncodedWord
transferEncodeEncodedWord (TransferDecodedEncodedWord charset lang s) =
  EncodedWord charset lang "Q" (review (clonePrism q) s)

-- | RFC 2047 and RFC 2231 define the /encoded-words/ mechanism for
-- embedding non-ASCII data in headers.  This function locates
-- encoded-words and decodes them.
--
-- @
-- λ> T.putStrLn $ decodeEncodedWords defaultCharsets "hello =?utf-8?B?5LiW55WM?=!"
-- hello 世界!
-- @
--
-- If parsing fails or the encoding is unrecognised the encoded-word
-- is left unchanged in the result.
--
-- @
-- λ> T.putStrLn $ decodeEncodedWords defaultCharsets "=?utf-8?B?bogus?="
-- =?utf-8?B?bogus?=
--
-- λ> T.putStrLn $ decodeEncodedWords defaultCharsets "=?utf-8?X?unrecognised_encoding?="
-- =?utf-8?X?unrecognised_encoding?=
-- @
--
-- Language specification is supported (the datum is discarded).
--
-- @
-- λ> T.putStrLn $ decodeEncodedWords defaultCharsets "=?utf-8*es?Q?hola_mundo!?="
-- hola mundo!
-- @
--
decodeEncodedWords :: CharsetLookup -> B.ByteString -> T.Text
decodeEncodedWords charsets s =
  either (const $ decodeLenient s) merge $ fmap (g . f) <$> parseOnly tokens s
  where
    -- parse the ByteString into a series of tokens
    -- of either raw ASCII text, or EncodedWords
    tokens = liftA2 (:) (Left <$> takeTillString "=?") more
          <|> ((:[]) . Left <$> takeByteString)
    more = liftA2 (:) (Right <$> encodedWord <|> pure (Left "=?")) tokens

    f (Left t) = Left t
    f (Right w) = first
      (const $ serialiseEncodedWord w :: TransferEncodingError -> B.ByteString)
      (view transferDecoded w)
    g (Left t) = Left t
    g (Right w) = first
      (const $ serialiseEncodedWord $ transferEncodeEncodedWord w :: CharsetError -> B.ByteString)
      (view (charsetDecoded charsets) w)

    merge = foldMap (either decodeLenient id)
