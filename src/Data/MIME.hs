{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
  , isAttachment
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
  , parseContentType
  , showContentType
  , mimeBoundary

  -- *** Content-Type values
  , contentTypeTextPlain
  , contentTypeApplicationOctetStream
  , contentTypeMultipartMixed
  , defaultContentType

  -- ** Content-Disposition header
  , contentDisposition
  , ContentDisposition(..)
  , DispositionType(..)
  , dispositionType
  , filename
  , filenameParameter

  -- ** Serialisation
  , renderMessage
  , buildMessage

  -- ** Mail creation
  , headerFrom
  , headerTo
  , headerCC
  , headerBCC
  , headerDate
  , createAttachmentFromFile
  , createAttachment
  , createTextPlainMessage
  , createMultipartMixedMessage

  -- * Re-exports
  , module Data.RFC5322
  , module Data.MIME.Parameter
  , module Data.MIME.Error
  ) where

import Control.Applicative
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import GHC.Generics (Generic)

import Control.DeepSeq (NFData)
import Control.Lens
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (char8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Builder as Builder
import Data.ByteString.Lazy (toStrict)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)

import Data.RFC5322
import Data.RFC5322.Internal
import Data.MIME.Error
import Data.MIME.Charset
import Data.MIME.EncodedWord
import Data.MIME.Parameter
import Data.MIME.TransferEncoding
import Data.MIME.Types (Encoding(..))

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

Create an inline, plain text message and render it:

@
renderMessage $ createTextPlainMessage "This is a test body"
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
  deriving (Eq, Show)

-- | Get all leaf entities from the MIME message
--
entities :: Traversal' MIMEMessage WireEntity
entities f (Message h a) = case a of
  Part b ->
    (\(Message h' b') -> Message h' (Part b')) <$> f (Message h b)
  Multipart bs ->
    Message h . Multipart <$> sequenceA (entities f <$> bs)

-- | Leaf entities with @Content-Disposition: attachment@
attachments :: Traversal' MIMEMessage WireEntity
attachments = entities . filtered (notNullOf l) where
  l = headers . contentDisposition . dispositionType . filtered (== Attachment)

-- | MIMEMessage content disposition is an 'Attachment'
isAttachment :: MIMEMessage -> Bool
isAttachment = has (headers . contentDisposition . dispositionType . filtered (== Attachment))

contentTransferEncoding :: Getter Headers TransferEncodingName
contentTransferEncoding = to $
  fromMaybe "7bit"
  . preview (header "content-transfer-encoding" . caseInsensitive)

instance HasTransferEncoding WireEntity where
  type TransferDecoded WireEntity = ByteEntity
  transferEncodingName = headers . contentTransferEncoding
  transferEncodedData = body
  transferDecoded = to $ \a -> (\t -> set body t a) <$> view transferDecodedBytes a

printContentTransferEncoding :: Encoding -> B.ByteString
printContentTransferEncoding Base64 = "base64"
printContentTransferEncoding None = "7bit"

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
  deriving (Show, Generic, NFData)

-- | Equality of Content-Type. Type and subtype are compared
-- case-insensitively and parameters are also compared.  Use
-- 'matchContentType' if you just want to match on the media type
-- while ignoring parameters.
--
instance Eq ContentType where
  ContentType a b c == ContentType a' b' c' = a == a' && b == b' && c == c'

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

printContentType :: ContentType -> B.ByteString
printContentType (ContentType typ sub params) =
  CI.original typ <> "/" <> CI.original sub <> printParameters params

printParameters :: Parameters -> B.ByteString
printParameters (Parameters xs) =
  foldMap (\(k,v) -> "; " <> CI.original k <> "=" <> v) xs

-- | Are the type and subtype the same? (parameters are ignored)
--
ctEq :: ContentType -> ContentType -> Bool
ctEq (ContentType typ1 sub1 _) = matchContentType typ1 (Just sub1)
{-# DEPRECATED ctEq "Use 'matchContentType' instead" #-}

ctType :: Lens' ContentType (CI B.ByteString)
ctType f (ContentType a b c) = fmap (\a' -> ContentType a' b c) (f a)

ctSubtype :: Lens' ContentType (CI B.ByteString)
ctSubtype f (ContentType a b c) = fmap (\b' -> ContentType a b' c) (f b)

ctParameters :: Lens' ContentType Parameters
ctParameters f (ContentType a b c) = fmap (\c' -> ContentType a b c') (f c)
{-# ANN ctParameters ("HLint: ignore Avoid lambda" :: String) #-}

-- | Rendered content type field value for displaying
showContentType :: ContentType -> T.Text
showContentType = decodeLenient . printContentType

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
    else pure $ ContentType typ subtype (Parameters params)

parseParameters :: Parser [(CI B.ByteString, B.ByteString)]
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
  charsetName = to $ \ent ->
    let
      (ContentType typ sub params) = view (headers . contentType) ent
      source = fromMaybe (InParameter (Just "us-ascii")) . (`lookup` textCharsetSources)
      l = rawParameter "charset" . caseInsensitive
    in
      if typ == "text"
      then case source sub of
        InBand f -> f (view body ent)
        InParameter def -> preview l params <|> def
        InBandOrParameter f def -> f (view body ent) <|> preview l params <|> def
      else
        preview l params <|> Just "us-ascii"
  charsetData = body -- XXX: do we need to drop the BOM / encoding decl?
  charsetDecoded = to $ \a -> (\t -> set body t a) <$> view charsetText a

  -- | Encode (@utf-8@) and add/set charset parameter.  If consisting
  -- entirely of ASCII characters, the @charset@ parameter gets set to
  -- @us-ascii@ instead of @utf-8@.
  --
  -- Ignores Content-Type (which is not correct for all content types).
  --
  charsetEncode (Message h a) =
    let
      b = T.encodeUtf8 a
      charset = if B.all (< 0x80) b then "us-ascii" else "utf-8"
    in Message (set (contentType . rawParameter "charset") charset h) b

-- | RFC 6657 provides for different media types having different
-- ways to determine the charset.  This data type defines how a
-- charset should be determined for some media type.
--
data EntityCharsetSource
  = InBand (B.ByteString -> Maybe CharsetName)
  -- ^ Charset should be declared within payload (e.g. xml, rtf).
  --   The given function reads it from the payload.
  | InParameter (Maybe CharsetName)
  -- ^ Charset should be declared in the 'charset' parameter,
  --   with optional fallback to the given default.
  | InBandOrParameter (B.ByteString -> Maybe CharsetName) (Maybe CharsetName)
  -- ^ Check in-band first, fall back to 'charset' parameter,
  --   and further optionally fall back to a default.

-- | Charset sources for text/* media types.  IANA registry:
-- https://www.iana.org/assignments/media-types/media-types.xhtml#text
--
textCharsetSources :: [(CI B.ByteString, EntityCharsetSource)]
textCharsetSources =
  [ ("plain", InParameter (Just "us-ascii"))
  , ("csv", InParameter (Just "utf-8"))
  , ("rtf", InBand (const (Just "us-ascii" {- TODO -})))

  -- https://tools.ietf.org/html/rfc2854
  -- The default is ambiguous; using us-ascii for now
  , ("html", InBandOrParameter (const Nothing {-TODO-}) (Just "us-ascii"))

  -- https://tools.ietf.org/html/rfc7763
  , ("markdown", InParameter Nothing)

  -- https://tools.ietf.org/html/rfc7303#section-3.2 and
  -- https://www.w3.org/TR/2008/REC-xml-20081126/#charencoding
  , ("xml", InBand (const (Just "utf-8") {-TODO-}))
  ]

-- | @text/plain; charset=us-ascii@
defaultContentType :: ContentType
defaultContentType =
  over parameterList (("charset", "us-ascii"):) contentTypeTextPlain

-- | @text/plain@
contentTypeTextPlain :: ContentType
contentTypeTextPlain = ContentType "text" "plain" (Parameters [])

-- | @application/octet-stream@
contentTypeApplicationOctetStream :: ContentType
contentTypeApplicationOctetStream =
  ContentType "application" "octet-stream" (Parameters [])

-- | @multipart/mixed; boundary=asdf@
contentTypeMultipartMixed :: B.ByteString -> ContentType
contentTypeMultipartMixed boundary =
  over parameterList (("boundary", boundary):)
  $ ContentType "multipart" "mixed" (Parameters [])

-- | Lens to the content-type header.  Probably not a lawful lens.
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
-- When setting, if the header already exists it is replaced,
-- otherwise it is added.  Unrecognised Content-Transfer-Encoding
-- is ignored when setting.
--
contentType :: Lens' Headers ContentType
contentType = lens sa sbt where
  sa s = case view cte s of
    Nothing -> contentTypeApplicationOctetStream
    Just _ ->
      fromMaybe defaultContentType $ preview (ct . parsed parseContentType) s

  sbt s b = set (at "Content-Type") (Just (printContentType b)) s

  ct = header "content-type"
  cte = contentTransferEncoding . to (`lookup` transferEncodings)

-- | Content-Disposition header (RFC 2183).
--
-- Use 'parameters' to access the parameters.
--
data ContentDisposition = ContentDisposition
  DispositionType   -- disposition
  Parameters        -- parameters
  deriving (Show, Generic, NFData)

data DispositionType = Inline | Attachment
  deriving (Eq, Show, Generic, NFData)

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
  <*> (Parameters <$> parseParameters)
  where
    mapDispType s
      | s == "inline" = Inline
      | otherwise = Attachment

printContentDisposition :: ContentDisposition -> B.ByteString
printContentDisposition (ContentDisposition typ params) =
  typStr <> printParameters params
  where
    typStr = case typ of Inline -> "inline" ; Attachment -> "attachment"

-- | Get Content-Disposition header.
-- Unrecognised disposition types are coerced to @Attachment@
-- in accordance with RFC 2183 §2.8 which states:
-- /Unrecognized disposition types should be treated as/ attachment.
--
-- The fold may be empty, e.g. if the header is absent or unparseable.
--
contentDisposition :: Traversal' Headers ContentDisposition
contentDisposition =
  header "content-disposition"
  . parsePrint parseContentDisposition printContentDisposition

-- | Traverse the value of the filename parameter (if present).
--
filename :: HasParameters a => Traversal' a T.Text
filename = filenameParameter . traversed . charsetPrism . value

-- | Access the filename parameter as a @Maybe ('ParameterValue' B.ByteString)@.
--
-- This can be used to read or set the filename parameter (see also
-- the 'newParameter' convenience function):
--
-- @
-- λ> let hdrs = Headers [("Content-Disposition", "attachment")]
-- λ> set ('contentDisposition' . 'filenameParameter') (Just ('newParameter' "foo.txt")) hdrs
-- Headers [("Content-Disposition","attachment; filename=foo.txt")]
-- @
filenameParameter :: HasParameters a => Lens' a (Maybe EncodedParameterValue)
filenameParameter = parameter "filename"


-- | Get the boundary, if specified
mimeBoundary :: Fold ContentType B.ByteString
mimeBoundary = parameters . rawParameter "boundary"

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
    case preview (rawParameter "boundary") ct of
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

-- | Serialise a given `MIMEMessage` into a ByteString. The message is
-- serialised as is. No additional headers are set.
renderMessage :: MIMEMessage -> B.ByteString
renderMessage = toStrict . Builder.toLazyByteString . buildMessage

-- | Serialise a given `MIMEMessage` using a `Builder`
buildMessage :: MIMEMessage -> Builder.Builder
buildMessage (Message h (Part partbody)) =
    buildFields h <> "\r\n" <> Builder.byteString partbody
buildMessage (Message h (Multipart xs)) =
    let b = firstOf (contentType . mimeBoundary) h
        boundary = maybe mempty (\b' -> "\r\n--" <> Builder.byteString b') b
        ents = foldMap (\part -> boundary <> "\r\n" <> buildMessage part) xs
    in buildFields h <> ents <> boundary <> "--\r\n"


mimeHeader :: (CI B.ByteString, B.ByteString)
mimeHeader = (CI.mk "MIME-Version", "1.0")

-- | creates a new `MIMEMessage` and transfer encodes the given content with the
-- given Encoding
createMessage
    :: ContentType
    -> ContentDisposition
    -> Encoding
    -> B.ByteString -- ^ content
    -> MIMEMessage
createMessage ct cd encoding content =
  let m = Message (Headers [mimeHeader]) (Part $ transferEncodeData encoding content)
  in m
  & set (headers . at "Content-Type") (Just (printContentType ct))
  . set (headers . at "Content-Disposition") (Just (printContentDisposition cd))
  . set (headers . at "Content-Transfer-Encoding") (Just (printContentTransferEncoding encoding))

headerFrom :: Lens' Headers [Mailbox]
headerFrom = lens getter setter
  where
    getter = either (pure []) id . parseOnly mailboxList . view (header "from")
    setter = flip $ set (header "from") . renderMailboxes

headerTo :: Lens' Headers [Address]
headerTo = lens (headerGetter "to") (headerSetter "to")

headerCC :: Lens' Headers [Address]
headerCC = lens (headerGetter "cc") (headerSetter "cc")

headerBCC :: Lens' Headers [Address]
headerBCC = lens (headerGetter "bcc") (headerSetter "bcc")

headerSetter :: CI B.ByteString -> Headers -> [Address] -> Headers
headerSetter fieldname = flip $ set (header fieldname) . renderAddresses

headerGetter :: CI C8.ByteString -> Headers -> [Address]
headerGetter fieldname =
    either (pure []) id . parseOnly addressList . view (header fieldname)

headerDate :: Lens' Headers UTCTime
headerDate = lens getter setter
  where
    -- TODO for parseTimeOrError. See #16
    getter =
        parseTimeOrError True defaultTimeLocale rfc5422DateTimeFormat
        . C8.unpack . view (header "date")
    setter hdrs x = set (header "date") (renderRFC5422Date x) hdrs

-- | Create a mixed `MIMEMessage` with an inline text/plain part and multiple
-- `attachments`
--
-- Additional headers can be set (e.g. 'cc') by using `At` and `Ixed`, for
-- example:
--
-- @
-- λ> set (at "subject") (Just "Hey there") $ Headers []
-- Headers [("subject", "Hey there")]
-- @
--
-- You can also use the `Mailbox` instances:
--
-- @
-- λ> let address = Mailbox (Just "roman") (AddrSpec "roman" (DomainLiteral "192.168.1.1"))
-- λ> set (at "cc") (Just $ renderMailbox address) $ Headers []
-- Headers [("cc", "\\"roman\\" <roman@192.168.1.1>")]
-- @
createMultipartMixedMessage
    :: B.ByteString -- ^ Boundary
    -> [MIMEMessage] -- ^ attachments
    -> MIMEMessage
createMultipartMixedMessage b attachments' =
    let hdrs =
            set
                (at "Content-Type")
                (Just $ printContentType (contentTypeMultipartMixed b)) $
            Headers [mimeHeader]
    in Message hdrs (Multipart attachments')

-- | Create an inline, text/plain, utf-8 encoded message
--
createTextPlainMessage
    :: T.Text -- ^ message body
    -> MIMEMessage
createTextPlainMessage =
    createMessage
        contentTypeTextPlain
        (ContentDisposition Inline $ Parameters [(CI.mk "charset", "utf-8")])
        None
    . T.encodeUtf8

-- | Create an attachment from a given file path.
-- Note: The filename content disposition is set to the given `FilePath`. For
-- privacy reasons, you can unset/change it. See `filename` for examples.
--
createAttachmentFromFile :: ContentType -> FilePath -> IO MIMEMessage
createAttachmentFromFile ct fp = createAttachment ct (Just fp) <$> B.readFile fp

-- | Create an attachment from the given file contents. Optionally set the given
-- filename parameter to the given file path.
--
createAttachment :: ContentType -> Maybe FilePath -> B.ByteString -> MIMEMessage
createAttachment ct fp =
    set
        (headers . contentDisposition . filenameParameter)
        (newParameter . T.pack <$> fp) .
    createMessage ct (ContentDisposition Attachment $ Parameters []) Base64
