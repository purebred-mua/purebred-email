{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |

@encoded-word@s for representing non-7bit ASCII data in headers
(RFC 2047 and RFC 2231).

-}
module Data.MIME.EncodedWord
  (
    decodeEncodedWord
  , decodeEncodedWords
  , EncodedWord
  , encodedWord
  , buildEncodedWord
  , encodeEncodedWords
  , chooseEncodedWordEncoding
  ) where

import Control.Applicative ((<|>), liftA2, optional)
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(Sum), Any(Any))

import Control.Lens (to, clonePrism, review, view, foldMapOf)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (char8)
import Data.ByteString.Lens (bytes)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as Builder
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.MIME.Charset
import Data.MIME.Error (EncodingError)
import Data.MIME.TransferEncoding
import Data.MIME.Base64
import Data.MIME.QuotedPrintable
import Data.RFC5322.Internal (ci, takeTillString)

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
serialiseEncodedWord = L.toStrict . Builder.toLazyByteString . buildEncodedWord

buildEncodedWord :: EncodedWord -> Builder.Builder
buildEncodedWord (EncodedWord charset lang enc s) =
  "=?" <> Builder.byteString (CI.original charset)
  <> maybe "" (\l -> "*" <> Builder.byteString (CI.original l)) lang
  <> "?" <> Builder.byteString (CI.original enc)
  <> "?" <> Builder.byteString s <> "?="


transferEncodeEncodedWord :: TransferDecodedEncodedWord -> EncodedWord
transferEncodeEncodedWord (TransferDecodedEncodedWord charset lang s) =
  let (enc, p) = fromMaybe ("Q", q) (chooseEncodedWordEncoding s)
  in EncodedWord charset lang enc (review (clonePrism p) s)

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
  either (const (decodeLenient s)) (foldMap conv) (parseOnly tokens s)
  where
    tokens :: Parser [Either B.ByteString EncodedWord]
    tokens = liftA2 (:) (Left <$> takeTillString "=?") more
          <|> ((:[]) . Left <$> takeByteString)
    more = liftA2 (:) (Right <$> encodedWord <|> pure (Left "=?")) tokens
    conv = either decodeLenient (decodeEncodedWord charsets)

-- | Decode an 'EncodedWord'.  If transfer or charset decoding fails,
-- returns the serialised encoded word.
decodeEncodedWord :: CharsetLookup -> EncodedWord -> T.Text
decodeEncodedWord charsets w =
  either
    (decodeLenient . const (serialiseEncodedWord w) :: EncodingError -> T.Text)
    id
    (view transferDecoded w >>= view (charsetDecoded charsets))

-- | This function encodes necessary parts of some text
--
-- Currently turns the whole text into a single encoded word, if necessary.
encodeEncodedWords :: T.Text -> B.ByteString
encodeEncodedWords t = maybe utf8 (const ew) (chooseEncodedWordEncoding utf8)
  where
    ew = B.intercalate " " . fmap encOrNot . B.split 32 $ utf8
    encOrNot s = maybe s (g s) (chooseEncodedWordEncoding s)
    g s (enc, p) = serialiseEncodedWord $
      EncodedWord "utf-8" Nothing enc (review (clonePrism p) s)
    utf8 = T.encodeUtf8 t

chooseEncodedWordEncoding :: B.ByteString -> Maybe (TransferEncodingName, TransferEncoding)
chooseEncodedWordEncoding s
  | not doEnc = Nothing
  | nQP < nB64 = Just ("Q", q)
  | otherwise = Just ("B", b)
  where
    -- https://tools.ietf.org/html/rfc5322#section-3.5 'text'
    needEnc c = c > 127 || c == 0
    qpBytes c
      | encodingRequiredNonEOL Q c = 3
      | otherwise = 1
    (Any doEnc, Sum nQP) = foldMapOf bytes (\c -> (Any (needEnc c), Sum (qpBytes c))) s
    nB64 = ((B.length s + 2) `div` 3) * 4
