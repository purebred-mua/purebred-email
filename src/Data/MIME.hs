{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- |

MIME messages (RFC 2045, RFC 2046 and friends).

-}
module Data.MIME
  (
  -- * Overview
  -- $overview

  -- * Examples / HOWTO
  -- $examples

  -- * API

  -- ** MIME data type
    MIME(..)
  , mime
  , MIMEMessage

  , WireEntity
  , ByteEntity
  , TextEntity
  , EncStateWire
  , EncStateByte

  -- *** Accessing and processing entities
  , entities
  , attachments
  , transferDecoded
  , charsetDecoded

  -- ** Header processing
  , decodeEncodedWords

  -- ** Content-Type header
  , contentType
  , ContentType(..)
  , ctType
  , ctSubtype
  , matchContentType
  , ctEq

  -- *** Content-Type values
  , contentTypeTextPlain
  , contentTypeApplicationOctetStream
  , defaultContentType

  -- ** Content-Disposition header
  , contentDisposition
  , ContentDisposition(..)
  , DispositionType(..)
  , dispositionType
  , filename

  -- * Re-exports
  , module Data.RFC5322
  , module Data.MIME.Error
  ) where

import Control.Applicative
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))

import Control.Lens
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (char8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T

import Data.RFC5322
import Data.RFC5322.Internal
import Data.MIME.Error
import Data.MIME.Charset
import Data.MIME.EncodedWord
import Data.MIME.Parameter
import Data.MIME.TransferEncoding

{- $overview

This module extends 'Data.RFC5322' with types for handling MIME
messages.  It provides the 'mime' parsing helper function for
use with 'message'.

@
'mime' :: Headers -> Parser MIME
message mime :: Parser (Message ctx MIME)
@

The 'Message' data type has a phantom type parameter for context.
In this module we use it to track whether the body has
/content transfer encoding/ or /charset encoding/ applied.  Type
aliases are provided for convenince.

@
data 'Message' ctx a = Message Headers a
data 'EncStateWire'
data 'EncStateByte'

type 'MIMEMessage' = Message EncStateWire MIME
type 'WireEntity' = Message EncStateWire B.ByteString
type 'ByteEntity' = Message EncStateByte B.ByteString
type 'TextEntity' = Message () T.Text
@

Folds are provided over all leaf /entities/, and entities that are
identified as attachments:

@
'entities' :: Fold MIMEMessage WireEntity
'attachments' :: Fold MIMEMessage WireEntity
@

Content transfer decoding is performed using the 'transferDecoded'
optic.  This will convert /Quoted-Printable/ or /Base64/ encoded
entities into their decoded form.

@
'transferDecoded' :: Getter WireEntity (Either 'TransferEncodingError' ByteEntity)
transferDecoded :: Getter WireEntity (Either 'EncodingError' ByteEntity)
transferDecoded :: (HasTransferEncoding a, AsTransferEncodingError e) => Getter a (Either e (TransferDecoded a))
@

Charset decoding is performed using the 'charsetDecoded' optic:

@
'charsetDecoded' :: Getter ByteEntity (Either 'CharsetError' TextEntity)
charsetDecoded :: Getter ByteEntity (Either 'EncodingError' TextEntity)
charsetDecoded :: (HasCharset a, AsCharsetError e) => Getter a (Either e (Decoded a))
@

-}

{- $examples

__Parse__ a MIME message:

@
'parse' (message mime) :: ByteString -> Either String MIMEMessage
@

Find the first entity with the @text/plain@ __content type__:

@
getTextPlain :: 'MIMEMessage' -> Maybe 'WireEntity'
getTextPlain = firstOf ('entities' . filtered f)
  where
  f = 'matchContentType' "text" (Just "plain") . view ('headers' . 'contentType')
@

Perform __content transfer decoding__ /and/ __charset__ decoding
while preserving decode errors:

@
view 'transferDecoded' >=> view 'charsetDecoded' :: WireEntity -> Either EncodingError TextEntity
@

Get all __attachments__ (transfer decoded) and their __filenames__ (if specified):

@
getAttachments :: 'MIMEMessage' -> [(Either 'TransferEncodingError' B.ByteString, Maybe T.Text)]
getAttachments = toListOf ('attachments' . to (liftA2 (,) content name)
  where
  content = view 'transferDecodedBytes'
  name = preview ('headers' . 'contentDisposition' . 'filename')
@

-}


-- | Entity is formatted for transfer.  Processing requires
-- transfer decoding.
--
data EncStateWire

-- | Entity requires content-transfer-encoding to send,
--   and may require charset decoding to read.
--
data EncStateByte

type MIMEMessage = Message EncStateWire MIME
type WireEntity = Message EncStateWire B.ByteString
type ByteEntity = Message EncStateByte B.ByteString
type TextEntity = Message () T.Text

-- | MIME message body.  Either a single @Part@, or @Multipart@.
-- Only the body is represented; preamble and epilogue are not.
--
data MIME
  = Part B.ByteString
  | Multipart [MIMEMessage]
  deriving (Show)

-- | Get all leaf entities from the MIME message
--
entities :: Fold MIMEMessage WireEntity
entities f (Message h a) = case a of
  Part b ->
    (\(Message h' b') -> Message h' (Part b')) <$> f (Message h b)
  Multipart bs ->
    Message h . Multipart <$> sequenceA (entities f <$> bs)

-- | Leaf entities with @Content-Disposition: attachment@
attachments :: Fold MIMEMessage WireEntity
attachments = entities . filtered (notNullOf l) where
  l = headers . contentDisposition . dispositionType . filtered (== Attachment)

contentTransferEncoding :: Getter Headers TransferEncodingName
contentTransferEncoding = to $
  fromMaybe "7bit"
  . preview (header "content-transfer-encoding" . caseInsensitive)

instance HasTransferEncoding WireEntity where
  type TransferDecoded WireEntity = ByteEntity
  transferEncodingName = headers . contentTransferEncoding
  transferEncodedData = body
  transferDecoded = to $ \a -> (\t -> set body t a) <$> view transferDecodedBytes a


caseInsensitive :: CI.FoldCase s => Iso' s (CI s)
caseInsensitive = iso CI.mk CI.original
{-# INLINE caseInsensitive #-}


-- | Content-Type header (RFC 2183).
-- Use 'parameters' to access the parameters.
-- Example:
--
-- @
-- ContentType "text" "plain" [("charset", "utf-8")]
-- @
--
data ContentType = ContentType (CI B.ByteString) (CI B.ByteString) Parameters
  deriving (Show)

-- | Match content type.  If @Nothing@ is given for subtype, any
-- subtype is accepted.
--
matchContentType
  :: CI B.ByteString         -- ^ type
  -> Maybe (CI B.ByteString) -- ^ optional subtype
  -> ContentType
  -> Bool
matchContentType wantType wantSubtype (ContentType gotType gotSubtype _) =
  wantType == gotType && maybe True (== gotSubtype) wantSubtype

-- | Are the type and subtype the same? (parameters are ignored)
--
ctEq :: ContentType -> ContentType -> Bool
ctEq (ContentType typ1 sub1 _) = matchContentType typ1 (Just sub1)
{-# DEPRECATED ctEq "Use 'matchContentType' instead" #-}

ctType :: Lens' ContentType (CI B.ByteString)
ctType f (ContentType a b c) = fmap (\a' -> ContentType a' b c) (f a)

ctSubtype :: Lens' ContentType (CI B.ByteString)
ctSubtype f (ContentType a b c) = fmap (\b' -> ContentType a b' c) (f b)

ctParameters :: Lens' ContentType [(CI B.ByteString, B.ByteString)]
ctParameters f (ContentType a b c) = fmap (\c' -> ContentType a b c') (f c)
{-# ANN ctParameters ("HLint: ignore Avoid lambda" :: String) #-}

instance HasParameters ContentType where
  parameters = ctParameters

-- | Parser for Content-Type header
parseContentType :: Parser ContentType
parseContentType = do
  typ <- ci token
  _ <- char8 '/'
  subtype <- ci token
  params <- parseParameters
  if typ == "multipart" && "boundary" `notElem` fmap fst params
    then
      -- https://tools.ietf.org/html/rfc2046#section-5.1.1
      fail "\"boundary\" parameter is required for multipart content type"
    else pure $ ContentType typ subtype params

parseParameters :: Parser Parameters
parseParameters = many (char8 ';' *> skipWhile (== 32 {-SP-}) *> param)
  where
    param = (,) <$> ci token <* char8 '=' <*> val
    val = token <|> quotedString

-- | header token parser
token :: Parser B.ByteString
token =
  takeWhile1 (\c -> c >= 33 && c <= 126 && notInClass "()<>@,;:\\\"/[]?=" c)

-- | RFC 2046 §4.1.2. defines the default character set to be US-ASCII.
--
instance HasCharset ByteEntity where
  type Decoded ByteEntity = TextEntity
  charsetName =
    let
      l = headers . contentType . parameters . rawParameter "charset" . caseInsensitive
    in
      to $ fromMaybe "us-ascii" . preview l
  charsetData = body
  charsetDecoded = to $ \a -> (\t -> set body t a) <$> view charsetText a


-- | @text/plain; charset=us-ascii@
defaultContentType :: ContentType
defaultContentType =
  over parameters (("charset", "us-ascii"):)
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
contentType = to $ \h -> case view cte h of
  Nothing -> contentTypeApplicationOctetStream
  Just _ ->
    fromMaybe defaultContentType $
    preview (header "content-type" . parsed parseContentType) h
  where
    cte = contentTransferEncoding . to (`lookup` transferEncodings)


-- | Content-Disposition header (RFC 2183).
--
-- Use 'parameters' to access the parameters.
--
data ContentDisposition = ContentDisposition
  DispositionType   -- disposition
  Parameters        -- parameters
  deriving (Show)

data DispositionType = Inline | Attachment
  deriving (Eq, Show)

dispositionType :: Lens' ContentDisposition DispositionType
dispositionType f (ContentDisposition a b) =
  fmap (\a' -> ContentDisposition a' b) (f a)
{-# ANN dispositionType ("HLint: ignore Avoid lambda" :: String) #-}

dispositionParameters :: Lens' ContentDisposition Parameters
dispositionParameters f (ContentDisposition a b) =
  fmap (\b' -> ContentDisposition a b') (f b)
{-# ANN dispositionParameters ("HLint: ignore Avoid lambda" :: String) #-}

instance HasParameters ContentDisposition where
  parameters = dispositionParameters

-- | Parser for Content-Disposition header
--
-- Unrecognised disposition types are coerced to @Attachment@
-- in accordance with RFC 2183 §2.8 which states: /Unrecognized disposition
-- types should be treated as /attachment//.
parseContentDisposition :: Parser ContentDisposition
parseContentDisposition = ContentDisposition
  <$> (mapDispType <$> ci token)
  <*> parseParameters
  where
    mapDispType s
      | s == "inline" = Inline
      | otherwise = Attachment

-- | Get Content-Disposition header.
-- Unrecognised disposition types are coerced to @Attachment@
-- in accordance with RFC 2183 §2.8 which states:
-- /Unrecognized disposition types should be treated as/ attachment.
--
-- The fold may be empty, e.g. if the header is absent or unparseable.
--
contentDisposition :: Fold Headers ContentDisposition
contentDisposition =
  header "content-disposition" . parsed parseContentDisposition

-- | Get the filename, if specified.
--
filename :: Fold ContentDisposition T.Text
filename = parameters . parameter "filename" . charsetText' . folded


-- | Top-level MIME body parser that uses headers to decide how to
--   parse the body.
--
-- __Do not use this parser for parsing a nested message.__
-- This parser should only be used when the message you want to
-- parse is the /whole input/.  If you use it to parse a nested
-- message it will treat the remainder of the outer message(s)
-- as part of the epilogue.
--
-- Preambles and epilogues are discarded.
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
    case preview (parameters . rawParameter "boundary") ct of
      Nothing -> part
      Just boundary -> Multipart <$> multipart takeTillEnd boundary
  _ -> part
  where
    part = Part <$> takeTillEnd

-- | Parse a multipart MIME message.  Preambles and epilogues are
-- discarded.
--
multipart
  :: Parser B.ByteString  -- ^ parser to the end of the part
  -> B.ByteString         -- ^ boundary, sans leading "--"
  -> Parser [MIMEMessage]
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
