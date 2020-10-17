{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- |

MIME messages (RFC 2045, RFC 2046, RFC 2183 and friends).

This module extends "Data.RFC5322" with types for handling MIME
messages.  It provides the 'mime' parsing helper function for
use with 'message'.

-}
module Data.MIME
  (
  -- * Overview / HOWTO
  -- ** Creating and serialising mail
  -- $create

  -- ** Parsing mail
  -- $parse

  -- ** Inspecting messages
  -- $inspect

  -- ** Unicode support
  -- $unicode

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
  , transferDecoded'
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
  , renderContentType
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
  , renderContentDisposition

  -- ** Mail creation
  -- *** Common use cases
  , createTextPlainMessage
  , createAttachment
  , createAttachmentFromFile
  , createMultipartMixedMessage
  , encapsulate
  -- *** Setting headers
  , headerFrom
  , headerTo
  , headerCC
  , headerBCC
  , headerDate
  , headerSubject
  , headerText
  , replyHeaderReferences

  -- * Re-exports
  , CharsetLookup
  , defaultCharsets
  , module Data.RFC5322
  , module Data.MIME.Parameter
  , module Data.MIME.Error
  ) where

import Control.Applicative
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty, fromList, intersperse)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Semigroup ((<>))
import Data.String (IsString(fromString))
import GHC.Generics (Generic)

import Control.DeepSeq (NFData)
import Control.Lens
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (char8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Builder as Builder
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)

import Data.RFC5322
import Data.RFC5322.Internal hiding (takeWhile1)
import Data.MIME.Error
import Data.MIME.Charset
import Data.MIME.EncodedWord
import Data.MIME.Parameter
import Data.MIME.TransferEncoding

{- $create

Create an __inline, plain text message__ and __render__ it:

@
λ> import Data.MIME
λ> msg = 'createTextPlainMessage' "Hello, world!"
λ> s = 'renderMessage' msg
λ> L.putStrLn s
MIME-Version: 1.0
Content-Transfer-Encoding: 7bit
Content-Disposition: inline
Content-Type: text/plain; charset=us-ascii

Hello, world!
@

Set the __@From@__ and __@To@__ headers:

@
λ> alice = Mailbox Nothing (AddrSpec "alice" (DomainDotAtom ("example" :| ["com"])))
λ> bob = Mailbox Nothing (AddrSpec "bob" (DomainDotAtom ("example" :| ["net"])))
λ> msgFromAliceToBob = set ('headerFrom' 'defaultCharsets' [alice] . set ('headerTo' defaultCharsets) [Single bob] $ msg
λ> L.putStrLn (renderMessage msgFromAliceToBob)
MIME-Version: 1.0
From: alice@example.com
To: bob@example.net
Content-Transfer-Encoding: 7bit
Content-Disposition: inline
Content-Type: text/plain; charset=us-ascii

Hello, world!
@

The 'headerFrom', 'headerTo', 'headerCC' and 'headerBCC' lenses are the most
convenient interface for reading and setting the __sender and recipient addresses__.
Note that you would usually not manually construct email addresses
manually as was done above.  Instead you would usually read it from another
email or configuration, or parse addresses from user input.

The __@Subject@__ header is set via 'headerSubject'.  __Other single-valued headers__
can be set via 'headerText'.

@
λ> :{
| L.putStrLn . renderMessage $
|   set ('headerText' defaultCharsets "Comments") (Just "와")
|   . set ('headerSubject' defaultCharsets) (Just "Hi from Alice")
|   $ msgFromAliceToBob
| :}

MIME-Version: 1.0
Comments: =?utf-8?B?7JmA?=
Subject: Hi from Alice
From: alice@example.com
To: bob@example.net
Content-Transfer-Encoding: 7bit
Content-Disposition: inline
Content-Type: text/plain; charset=us-ascii

Hello, world!
@

Create a __multipart message with attachment__:

@
λ> attachment = 'createAttachment' "application/json" (Just "data.json") "{\"foo\":42}"
λ> msg2 = 'createMultipartMixedMessage' "boundary" [msg, attachment]
λ> s2 = 'renderMessage' msg2
λ> L.putStrLn s2
MIME-Version: 1.0
Content-Type: multipart/mixed; boundary=boundary

--boundary
Content-Transfer-Encoding: 7bit
Content-Disposition: inline
Content-Type: text/plain; charset=us-ascii

Hello, world!
--boundary
Content-Transfer-Encoding: 7bit
Content-Disposition: attachment; filename=data.json
Content-Type: application/json

{"foo":42}
--boundary--

@

__NOTE:__ if you only need to write a serialised 'Message' to an 
IO handle, 'buildMessage' is more efficient than 'renderMessage'.

-}

{- $parse

Most often you will __parse a message__ like this:

@
λ> parsedMessage = 'parse' ('message' 'mime') s2
λ> :t parsedMessage
parsedMessage :: Either String 'MIMEMessage'
λ> parsedMessage == Right msg2
True
@

The 'message' function builds a parser for a message.  It is
abstracted over the body type; the argument is a function that can
inspect headers and return a parser for the body.  If you are
parsing MIME messages (or plain RFC 5322 messages), the 'mime'
function is the right one to use.

-}

{- $inspect

Parsing an email is nice, but your normally want to get at the
content inside.  One of the most important tasks is __finding entities__
of interest, e.g. attachments, plain text or HTML bodies.  The
'entities' optic is a fold over all /leaf/ entities in the message.
That is, all the non-multipart bodies.  You can use 'filtered' to
refine the query.

For example, let's say you want to find the first @text/plain@
entity in a message.  Define a predicate with the help of the
'matchContentType' function:

@
λ> isTextPlain = 'matchContentType' "text" (Just "plain") . view 'contentType'
λ> :t isTextPlain
isTextPlain :: HasHeaders s => s -> Bool
λ> isTextPlain msg
True
λ> isTextPlain msg2
False
@

Now we can use the predicate to construct a fold and retrieve the
body.  If there is no matching entity the result would be @Nothing@.

@
λ> firstOf ('entities' . filtered isTextPlain . 'body') msg2
Just "Hello, world!"
@

For __attachments__ you are normally interested in the binary data
and possibly the filename (if specified).  In the following example
we retrieve all attachments, and their filenames, as a list of
tuples (although there is only one in the message).  Note that

Get the (optional) filenames and (decoded) body of all attachments,
as a list of tuples.  The 'attachments' optic selects non-multipart
entities with @Content-Disposition: attachment@.  The 'attachments'
fold targets all entities with @Content-Disposition: attachment@.
The 'transferDecoded'' optic undoes the @Content-Transfer-Encoding@
of the entity.

@
λ> getFilename = preview ('contentDisposition' . _Just . 'filename' 'defaultCharsets')
λ> getBody = preview ('transferDecoded'' . _Right . 'body')
λ> getAttachment = liftA2 (,) getFilename getBody
λ> toListOf ('attachments' . to getAttachment) msg2
[(Just "data.json",Just "{\"foo\":42}")]
@

Finally, note that the 'filename' optic takes an argument: it is a
function for looking up a character set.  Supporting every possible
character encoding is a bit tricky so we let the user supply a map
of supported charsets, and provide 'defaultCharsets' which supports
ASCII, UTF-8 and ISO-8859-1.

@
λ> :t 'filename'
filename
  :: ('HasParameters' a, Applicative f) =>
     'CharsetLookup' -> (T.Text -> f T.Text) -> a -> f a
λ> :t 'defaultCharsets'
defaultCharsets :: CharsetLookup
λ> :i CharsetLookup
type CharsetLookup = CI Char8.ByteString -> Maybe Data.MIME.Charset.Charset
@

-}

{- $unicode

In Australia we say "Hello world" upside down:

@
λ> msg3 = createTextPlainMessage "ɥǝןןo ʍoɹןp"
λ> L.putStrLn $ renderMessage msg3
MIME-Version: 1.0
Content-Transfer-Encoding: base64
Content-Disposition: inline
Content-Type: text/plain; charset=utf-8

yaXHndef159vIMqNb8m5159w

@

Charset set and transfer encoding are handled automatically.  If the
message only includes characters representable in ASCII, the charset
will be @us-ascii@, otherwise @utf-8@.

To read the message as @Text@ you must perform transfer decoding and
charset decoding.  The 'transferDecoded' optic performs transfer
decoding, as does its sibling 'transferDecoded'' which is
monomorphic in the error type.  Similarly, 'charsetText' and
'charsetText'' perform text decoding according to the character set.

If you don't mind throwing away decoding errors, the simplest way to
get the text of a message is:

@
λ> Just ent = firstOf ('entities' . filtered isTextPlain) msg3
λ> :t ent
ent :: 'WireEntity'
λ> text = preview ('transferDecoded'' . _Right . 'charsetText'' 'defaultCharsets' . _Right) ent
λ> :t text
text :: Maybe T.Text
λ> traverse_ T.putStrLn text
ɥǝןןo ʍoɹןp
@

As mentioned earlier, functions that perform text decoding take a
'CharsetLookup' parameter, and we provide 'defaultCharsets' for
convenience.

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
  | Encapsulated MIMEMessage
  | Multipart (NonEmpty MIMEMessage)
  | FailedParse MIMEParseError B.ByteString
  deriving (Eq, Show)

-- | Ignores the presence/absense of @MIME-Version@ header
instance EqMessage MIME where
  Message h1 b1 `eqMessage` Message h2 b2 =
    stripVer h1 == stripVer h2 && b1 == b2
    where
    stripVer = set (headers . at "MIME-Version") Nothing

-- | Get all leaf entities from the MIME message.
-- Entities that failed to parse are skipped.
--
entities :: Traversal' MIMEMessage WireEntity
entities f (Message h a) = case a of
  Part b ->
    (\(Message h' b') -> Message h' (Part b')) <$> f (Message h b)
  Encapsulated msg -> Message h . Encapsulated <$> entities f msg
  Multipart bs ->
    Message h . Multipart <$> sequenceA (entities f <$> bs)
  FailedParse _ _ -> pure (Message h a)

-- | Leaf entities with @Content-Disposition: attachment@
attachments :: Traversal' MIMEMessage WireEntity
attachments = entities . filtered isAttachment

-- | MIMEMessage content disposition is an 'Attachment'
isAttachment :: HasHeaders a => a -> Bool
isAttachment = has (contentDisposition . _Just . dispositionType . filtered (== Attachment))

contentTransferEncoding
  :: (Profunctor p, Contravariant f) => Optic' p f Headers TransferEncodingName
contentTransferEncoding = to $
  fromMaybe "7bit"
  . preview (header "content-transfer-encoding" . caseInsensitive)

instance HasTransferEncoding WireEntity where
  type TransferDecoded WireEntity = ByteEntity
  transferEncodingName = headers . contentTransferEncoding
  transferEncodedData = body
  transferDecoded = to $ \a -> (\t -> set body t a) <$> view transferDecodedBytes a

  transferEncode (Message h s) =
    let
      (cteName, cte) = chooseTransferEncoding s
      s' = review (clonePrism cte) s
      cteName' = CI.original cteName
      h' = set (headers . at "Content-Transfer-Encoding") (Just cteName') h
    in
      Message h' s'

caseInsensitive :: CI.FoldCase s => Iso' s (CI s)
caseInsensitive = iso CI.mk CI.original
{-# INLINE caseInsensitive #-}


-- | Content-Type header (RFC 2183).
-- Use 'parameters' to access the parameters.
-- Example:
--
-- @
-- ContentType "text" "plain" (Parameters [("charset", "utf-8")])
-- @
--
-- You can also use @-XOverloadedStrings@ but be aware the conversion
-- is non-total (throws an error if it cannot parse the string).
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

-- | __NON-TOTAL__ parses the Content-Type (including parameters)
-- and throws an error if the parse fails
--
instance IsString ContentType where
  fromString = either err id . parseOnly parseContentType . C8.pack
    where
    err msg = error $ "failed to parse Content-Type: " <> msg

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

renderContentType :: ContentType -> B.ByteString
renderContentType (ContentType typ sub params) =
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
showContentType = decodeLenient . renderContentType

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
  charsetDecoded m = to $ \a -> (\t -> set body t a) <$> view (charsetText m) a

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
    in Message (set (contentType . parameter "charset") (Just charset) h) b

-- | RFC 6657 provides for different media types having different
-- ways to determine the charset.  This data type defines how a
-- charset should be determined for some media type.
--
data EntityCharsetSource
  = InBand (B.ByteString -> Maybe CharsetName)
  -- ^ Charset should be declared within payload (e.g. xml, rtf).
  --   The given function reads it from the payload.
  | InParameter (Maybe CharsetName)
  -- ^ Charset should be declared in the @charset@ parameter,
  --   with optional fallback to the given default.
  | InBandOrParameter (B.ByteString -> Maybe CharsetName) (Maybe CharsetName)
  -- ^ Check in-band first, fall back to @charset@ parameter,
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
contentTypeTextPlain = ContentType "text" "plain" mempty

-- | @application/octet-stream@
contentTypeApplicationOctetStream :: ContentType
contentTypeApplicationOctetStream =
  ContentType "application" "octet-stream" mempty

-- | @multipart/mixed; boundary=asdf@
contentTypeMultipartMixed :: B.ByteString -> ContentType
contentTypeMultipartMixed boundary =
  set (parameter "boundary") (Just (ParameterValue Nothing Nothing boundary))
  $ ContentType "multipart" "mixed" mempty

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
contentType :: HasHeaders a => Lens' a ContentType
contentType = headers . lens sa sbt where
  sa s = case view cte s of
    Nothing -> contentTypeApplicationOctetStream
    Just _ ->
      fromMaybe defaultContentType $ preview (ct . parsed parseContentType) s

  sbt s b = set (at "Content-Type") (Just (renderContentType b)) s

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

-- | Render the Content-Disposition value, including parameters.
renderContentDisposition :: ContentDisposition -> B.ByteString
renderContentDisposition (ContentDisposition typ params) =
  typStr <> printParameters params
  where
    typStr = case typ of Inline -> "inline" ; Attachment -> "attachment"

-- | Access @Content-Disposition@ header.
--
-- Unrecognised disposition types are coerced to @Attachment@
-- in accordance with RFC 2183 §2.8 which states:
-- /Unrecognized disposition types should be treated as attachment/.
--
-- This optic does not distinguish between missing header or malformed
-- value.
--
contentDisposition :: HasHeaders a => Lens' a (Maybe ContentDisposition)
contentDisposition = headers . at "Content-Disposition" . dimap
  (>>= either (const Nothing) Just . Data.RFC5322.parse parseContentDisposition)
  (fmap . fmap $ renderContentDisposition)

-- | Traverse the value of the filename parameter (if present).
--
filename :: HasParameters a => CharsetLookup -> Traversal' a T.Text
filename m = filenameParameter . traversed . charsetPrism m . value

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
mimeBoundary :: Traversal' ContentType B.ByteString
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
-- This parser accepts non-MIME messages, and
-- treats them as a single part.
--
mime :: Headers -> BodyHandler MIME
mime h
  | nullOf (header "MIME-Version") h = RequiredBody (Part <$> takeByteString)
  | otherwise = mime' takeByteString h

type instance MessageContext MIME = EncStateWire

mime'
  :: Parser B.ByteString
  -- ^ Parser FOR A TAKE to the part delimiter.  If this part is
  -- multipart, we pass it on to the 'multipart' parser.  If this
  -- part is not multipart, we just do the take.
  -> Headers
  -> BodyHandler MIME
mime' takeTillEnd h = RequiredBody $ case view contentType h of
  ct | view ctType ct == "multipart" ->
    case preview (rawParameter "boundary") ct of
      Nothing -> FailedParse MultipartBoundaryNotSpecified <$> takeTillEnd
      Just boundary ->
        (Multipart <$> multipart takeTillEnd boundary)
        <|> (FailedParse MultipartParseFail <$> takeTillEnd)
     | matchContentType "message" (Just "rfc822") ct ->
        (Encapsulated <$> message (mime' takeTillEnd))
        <|> (FailedParse EncapsulatedMessageParseFail <$> takeTillEnd)
  _ -> part
  where
    part = Part <$> takeTillEnd

data MIMEParseError
  = MultipartBoundaryNotSpecified
  | MultipartParseFail
  | EncapsulatedMessageParseFail
  deriving (Eq, Show)

-- | Parse a multipart MIME message.  Preambles and epilogues are
-- discarded.
--
multipart
  :: Parser B.ByteString  -- ^ parser to the end of the part
  -> B.ByteString         -- ^ boundary, sans leading "--"
  -> Parser (NonEmpty MIMEMessage)
multipart takeTillEnd boundary =
  skipTillString dashBoundary *> crlf -- FIXME transport-padding
  *> fmap fromList (part `sepBy1` crlf)
  <* string "--" <* takeTillEnd
  where
    delimiter = "\n--" <> boundary
    dashBoundary = B.tail delimiter
    part = message (mime' (trim <$> takeTillString delimiter))
    trim s  -- trim trailing CR, because we only searched for LF
      | B.null s = s
      | C8.last s == '\r' = B.init s
      | otherwise = s

-- | Sets the @MIME-Version: 1.0@ header.
--
instance RenderMessage MIME where
  tweakHeaders = set (headers . at "MIME-Version") (Just "1.0")
  buildBody h z = Just $ case z of
    Part partbody -> Builder.byteString partbody
    Encapsulated msg -> buildMessage msg
    Multipart xs ->
      let b = firstOf (contentType . mimeBoundary) h
          boundary = maybe mempty (\b' -> "--" <> Builder.byteString b') b
      in
        boundary <> "\r\n"
        <> fold (intersperse ("\r\n" <> boundary <> "\r\n") (fmap buildMessage xs))
        <> "\r\n" <> boundary <> "--\r\n"
    FailedParse _ bs -> Builder.byteString bs



-- | Map a single-occurrence header to a list value.
-- On read, absent header is mapped to empty list.
-- On write, empty list results in absent header.
--
headerSingleToList
  :: (HasHeaders s)
  => (B.ByteString -> [a])
  -> ([a] -> B.ByteString)
  -> CI B.ByteString
  -> Lens' s [a]
headerSingleToList f g k =
  headers . at k . iso (maybe [] f) (\l -> if null l then Nothing else Just (g l))

headerFrom :: HasHeaders a => CharsetLookup -> Lens' a [Mailbox]
headerFrom charsets = headerSingleToList
  (either (const []) id . parseOnly (mailboxList charsets))
  renderMailboxes
  "From"

headerAddressList :: (HasHeaders a) => CI B.ByteString -> CharsetLookup -> Lens' a [Address]
headerAddressList k charsets = headerSingleToList
  (either (const []) id . parseOnly (addressList charsets))
  renderAddresses
  k

headerTo, headerCC, headerBCC :: (HasHeaders a) => CharsetLookup -> Lens' a [Address]
headerTo = headerAddressList "To"
headerCC = headerAddressList "Cc"
headerBCC = headerAddressList "Bcc"

headerDate :: HasHeaders a => Lens' a (Maybe UTCTime)
headerDate = headers . at "Date" . iso (parseDate =<<) (fmap renderRFC5422Date)
  where
    parseDate =
        parseTimeM True defaultTimeLocale rfc5422DateTimeFormatLax . C8.unpack

-- | Single-valued header with @Text@ value via encoded-words.
-- The conversion to/from Text is total (encoded-words that failed to be
-- decoded are passed through unchanged).  Therefore @Nothing@ means that
-- the header was not present.
--
-- This function is suitable for the @Subject@ header.
--
headerText :: (HasHeaders a) => CharsetLookup -> CI B.ByteString -> Lens' a (Maybe T.Text)
headerText charsets k =
  headers . at k . iso (fmap (decodeEncodedWords charsets)) (fmap encodeEncodedWords)

-- | Subject header.  See 'headerText' for details of conversion to @Text@.
headerSubject :: (HasHeaders a) => CharsetLookup -> Lens' a (Maybe T.Text)
headerSubject charsets = headerText charsets "Subject"


-- | Returns a space delimited `B.ByteString` with values from identification
-- fields from the parents message `Headers`. Rules to gather the values are in
-- accordance to RFC5322 - 3.6.4 as follows sorted by priority (first has
-- precedence):
--
-- * Values from @References@ and @Message-ID@ (if any)
-- * Values from @In-Reply-To@ and @Message-ID@ (if any)
-- * Value from @Message-ID@ (in case it's the first reply to a parent mail)
-- * Otherwise @Nothing@ is returned indicating that the replying mail should
--   not have a @References@ field.
--
replyHeaderReferences :: HasHeaders a => Getter a (Maybe C8.ByteString)
replyHeaderReferences = (.) headers $ to $ \hdrs ->
  let xs = catMaybes
        [preview (header "references") hdrs
         <|> preview (header "in-reply-to") hdrs
        , preview (header "message-id") hdrs
        ]
  in if null xs then Nothing else Just (B.intercalate " " xs)

-- | Create a mixed `MIMEMessage` with an inline text/plain part and multiple
-- `attachments`
--
createMultipartMixedMessage
    :: B.ByteString -- ^ Boundary
    -> NonEmpty MIMEMessage -- ^ parts
    -> MIMEMessage
createMultipartMixedMessage b attachments' =
    let hdrs = mempty &
                set contentType (contentTypeMultipartMixed b)
    in Message hdrs (Multipart attachments')

-- | Create an inline, text/plain, utf-8 encoded message
--
createTextPlainMessage :: T.Text -> MIMEMessage
createTextPlainMessage s = fmap Part $ transferEncode $ charsetEncode msg
  where
  msg = Message hdrs s :: TextEntity
  cd = ContentDisposition Inline mempty
  hdrs = mempty
          & set contentType contentTypeTextPlain
          & set contentDisposition (Just cd)

-- | Create an attachment from a given file path.
-- Note: The filename content disposition is set to the given `FilePath`. For
-- privacy reasons, you can unset/change it. See `filename` for examples.
--
createAttachmentFromFile :: ContentType -> FilePath -> IO MIMEMessage
createAttachmentFromFile ct fp = createAttachment ct (Just fp) <$> B.readFile fp

-- | Create an attachment from the given file contents. Optionally set the
-- filename parameter to the given file path.
--
createAttachment :: ContentType -> Maybe FilePath -> B.ByteString -> MIMEMessage
createAttachment ct fp s = Part <$> transferEncode msg
  where
  msg = Message hdrs s
  cd = ContentDisposition Attachment cdParams
  cdParams = mempty & set filenameParameter (newParameter <$> fp)
  hdrs = mempty
          & set contentType ct
          & set contentDisposition (Just cd)

-- | Encapsulate a message as a @message/rfc822@ message.
-- You can use this in creating /forwarded/ or /bounce/ messages.
--
encapsulate :: MIMEMessage -> MIMEMessage
encapsulate = Message hdrs . Encapsulated
  where
  hdrs = mempty & set contentType "message/rfc822"
