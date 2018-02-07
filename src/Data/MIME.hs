{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{- |

MIME messages (RFC 2045, RFC 2046 and friends).

There are different approaches to parsing the MIME content bodies,
to account for different use cases.

- Parse a body into a @ByteString@ (transfer encoding ignored)

- Parse a body into start offset and length.  The content is
  not included in the parsed data.  This mode is suitable e.g.
  for attachments, where there is no point reading the data into
  the program but you need enough information to read the body
  again at a later time.

The parser is configured with a function that tells it which body
type to use for a given part.  Multipart messages are handled
specially, as part of the 'MIME' data type.

-}
module Data.MIME
  (
  -- * MIME data
    MIME(..)
  , mime

  , Entity
  , entities
  , contentTransferDecoded

  -- * Content-Type header
  , ContentType(..)
  , ctType
  , ctSubtype
  , ctParameters
  , ctEq
  , contentType

  -- ** Content-Type values
  , contentTypeTextPlain
  , contentTypeApplicationOctetStream
  , defaultContentType

  -- * Re-exports
  , module Data.RFC5322
  ) where

import Control.Applicative

import Control.Lens
import Control.Monad (void)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (char8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.CaseInsensitive (mk)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))

import Data.RFC5322
import Data.RFC5322.Internal
import Data.MIME.Types
import Data.MIME.Base64
import Data.MIME.QuotedPrintable

type Entity = (Headers, B.ByteString)

data MIME
  = Part B.ByteString
  | Multipart [Message MIME]
  deriving (Show)

-- | Get all terminal entities from the MIME message
--
entities :: Fold (Message MIME) Entity
entities f (Message h a) = case a of
  Part b ->
    (\(h', b') -> Message h' (Part b')) <$> f (h, b)
  Multipart bs ->
    Message h . Multipart <$> sequenceA (entities f <$> bs)

-- | Decode an entity according to its Content-Transfer-Encoding
--
contentTransferDecoded :: Fold Entity B.ByteString
contentTransferDecoded = to f . folded
  where
  f (h, b) = (`preview` b) . clonePrism =<< contentTransferEncoding h


-- | Get the Content-Transfer-Encoding for an entity.
-- Defaults to @7bit@ (RFC 2045 §6.1) if the header is
-- not present.  Fails on /unrecognised/ values.
--
contentTransferEncoding :: Headers -> Maybe ContentTransferEncoding
contentTransferEncoding h = lookup (mk v) table
  where
    v = fromMaybe "7bit" $ preview (header "content-transfer-encoding") h
    table =
      [ ("7bit", id)
      , ("8bit", id)
      , ("binary", id)
      , ("quoted-printable", contentTransferEncodingQuotedPrintable)
      , ("base64", contentTransferEncodingBase64)
      ]


data ContentType = ContentType
  (CI B.ByteString) -- type
  (CI B.ByteString) -- subtype
  [(CI B.ByteString, B.ByteString)]  -- parameters
  deriving (Show)

-- | Are the type and subtype the same? (parameters are ignored)
ctEq :: ContentType -> ContentType -> Bool
ctEq (ContentType typ1 sub1 _) (ContentType typ2 sub2 _) =
  typ1 == typ2 && sub1 == sub2

ctType :: Lens' ContentType (CI B.ByteString)
ctType f (ContentType a b c) = fmap (\a' -> ContentType a' b c) (f a)

ctSubtype :: Lens' ContentType (CI B.ByteString)
ctSubtype f (ContentType a b c) = fmap (\b' -> ContentType a b' c) (f b)

ctParameters :: Lens' ContentType [(CI B.ByteString, B.ByteString)]
ctParameters f (ContentType a b c) = fmap (\c' -> ContentType a b c') (f c)
{-# ANN ctParameters ("HLint: ignore Avoid lambda" :: String) #-}

-- | Parser for Content-Type header
parseContentType :: Parser ContentType
parseContentType = do
  typ <- ci token
  _ <- char8 '/'
  subtype <- ci token
  params <- many (char8 ';' *> skipWhile (== 32 {-SP-}) *> parameter)
  if typ == "multipart" && "boundary" `notElem` fmap fst params
    then
      -- https://tools.ietf.org/html/rfc2046#section-5.1.1
      fail "\"boundary\" parameter is required for multipart content type"
    else pure $ ContentType typ subtype params
  where
    parameter = (,) <$> ci token <* char8 '=' <*> value
    value = token <|> quotedString
    token = takeWhile1 (\c -> c >= 33 && c <= 126 && notInClass "()<>@,;:\\\"/[]?=" c)


-- | @text/plain; charset=us-ascii@
defaultContentType :: ContentType
defaultContentType =
  over ctParameters (("charset", "us-ascii"):)
  contentTypeTextPlain

-- | @text/plain@
contentTypeTextPlain :: ContentType
contentTypeTextPlain = ContentType "text" "plain" []

-- | @application/octet-stream@
contentTypeApplicationOctetStream :: ContentType
contentTypeApplicationOctetStream =
  ContentType "application" "octet-stream" []

-- | Get the content-type header.
--
-- If the header is not specified or is syntactically invalid,
-- 'defaultContentType' is used.  For more info see
-- <https://tools.ietf.org/html/rfc2045#section-5.2>.
--
-- If the Content-Transfer-Encoding is unrecognised, the
-- actual Content-Type value is ignored and
-- @application/octet-stream@ is returned, as required by
-- <https://tools.ietf.org/html/rfc2049#section-2>.
--
contentType :: Getter Headers ContentType
contentType = to $ \h -> case contentTransferEncoding h of
  Nothing -> contentTypeApplicationOctetStream
  Just _ ->
    fromMaybe defaultContentType $
    preview (header "content-type" . parsed parseContentType) h


-- | Top-level MIME body parser that uses headers to decide how to
--   parse the body.
--
-- This parser can only be used at the top level.
-- __Do not use this parser for parsing a nested message.__
--
-- This parser accepts non-MIME messages, and unconditionally
-- treats them as a single part.
--
mime :: Headers -> Parser MIME
mime h
  | nullOf (header "MIME-Version") h = Part <$> takeByteString
  | otherwise = mime' takeByteString h

mime'
  :: Parser B.ByteString
  -- ^ Parser FOR A TAKE to the part delimiter.  If this part is
  -- multipart, we pass it on to the 'multipart' parser.  If this
  -- part is not multipart, we just do the take.
  -> Headers
  -> Parser MIME
mime' takeTillEnd h = case view contentType h of
  ct | view ctType ct == "multipart" ->
    case preview (ctParameters . header "boundary") ct of
      Nothing -> part
      Just boundary -> Multipart <$> multipart takeTillEnd boundary
  _ -> part
  where
    part = Part <$> takeTillEnd

{-

The multipart parser makes a few opinionated decisions.

- Preamble and epilogue are discarded

- Preamble and epilogue are assumed to be short, therefore
  the cost of skipping over these is also assumed to be low
  (until proven otherwise)

-}
multipart
  :: Parser B.ByteString  -- ^ parser to the end of the part
  -> B.ByteString         -- ^ boundary, sans leading "--"
  -> Parser [Message MIME]
multipart takeTillEnd boundary =
  multipartBody
  where
    delimiter = "\n--" <> boundary
    dashBoundary = B.tail delimiter
    part = message (mime' (trim <$> takeTillString delimiter))
    trim s  -- trim trailing CR, because we only searched for LF
      | B.null s = s
      | C8.last s == '\r' = B.init s
      | otherwise = s

    multipartBody =
      skipTillString dashBoundary *> crlf -- FIXME transport-padding
      *> part `sepBy` crlf
      <* string "--" <* crlf <* void takeTillEnd
