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

import Control.Lens (clonePrism, preview, to, view)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (char8)
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T

import Data.MIME.Charset
import Data.MIME.Types
import Data.MIME.Base64
import Data.MIME.QuotedPrintable
import Data.RFC5322.Internal

data EncodedWord = EncodedWord
  { _encodedWordCharset :: CI.CI B.ByteString
  , _encodedWordLanguage :: Maybe (CI.CI B.ByteString)
  , _encodedWordEncoding :: CI.CI B.ByteString
  , _encodedWordText :: B.ByteString
  }

data TransferDecodedEncodedWord = TransferDecodedEncodedWord
  { _transDecWordCharset :: CI.CI B.ByteString
  , _transDecWordLanguage :: Maybe (CI.CI B.ByteString)
  , _transDecWordText :: B.ByteString
  }

instance HasCharset TransferDecodedEncodedWord where
  type Decoded TransferDecodedEncodedWord = T.Text
  charsetName = to (pure . _transDecWordCharset)
  charsetData = to _transDecWordText
  charsetDecoded = charsetText


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

-- | Decode an encoded-word.  If the encoding or is not recognised
-- or if decoding fails, returns the serialised encoded-word.
--
transferDecodeEncodedWord :: EncodedWord -> Maybe TransferDecodedEncodedWord
transferDecodeEncodedWord (EncodedWord charset lang encName s) = do
  enc <- lookup encName encodedWordEncodings
  TransferDecodedEncodedWord charset lang <$> preview (clonePrism enc) s


encodedWordEncodings :: [(CI.CI B.ByteString, EncodedWordEncoding)]
encodedWordEncodings = [("Q", q), ("B", b)]

b :: EncodedWordEncoding
b = contentTransferEncodingBase64


decodeEncodedWords :: B.ByteString -> T.Text
decodeEncodedWords s = either (const $ decodeLenient s) merge $ do
  toks <- parseOnly tokens s
  transferDecTokens <- (traverse . traverse)
    (maybe (Left "transfer decoding failed") Right . transferDecodeEncodedWord)
    toks
  (traverse . traverse)
    (first show . view charsetDecoded)
    transferDecTokens
  where
    -- parse the ByteString into a series of tokens
    -- of either raw ASCII text, or EncodedWords
    tokens = liftA2 (:) (Left <$> takeTillString "=?") f
          <|> ((:[]) . Left <$> takeByteString)
    f = liftA2 (:) (Right <$> encodedWord <|> pure (Left "=?")) tokens

    merge = foldMap (either decodeLenient id)
