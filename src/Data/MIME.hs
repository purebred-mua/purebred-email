-- This file is part of purebred-email
-- Copyright (C) 2017-2021  Fraser Tweedale
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

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{- |

MIME messages (RFC 2045, RFC 2046, RFC 2183 and friends).

This module extends "Data.IMF" with types for handling MIME
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
  , charsetDecoded'

  -- ** Header processing
  , decodeEncodedWords

  -- ** Content-Type header
  , contentType
  , ContentType(..)
  , ctType
  , ctSubtype
  , matchContentType
  , parseContentType
  , renderContentType
  , showContentType

  -- *** @multipart@ media type
  , MultipartSubtype(..)

  -- **** @boundary@ parameter
  , Boundary
  , makeBoundary
  , unBoundary
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
  , setTextPlainBody

  -- *** Forward
  , encapsulate

  -- * Re-exports
  , CharsetLookup
  , defaultCharsets
  , module Data.IMF
  , module Data.MIME.Parameter
  , module Data.MIME.Error
  ) where

import Control.Applicative
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty, fromList, intersperse)
import Data.Maybe (fromMaybe)
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

import Data.IMF
import Data.IMF.Syntax hiding (takeWhile1)
import Data.MIME.Boundary
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
λ> attachment = 'createAttachment' "application/json" (Just "data.json") "{\\"foo\\":42}"
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

data MultipartSubtype
  = Mixed
  -- ^ <https://www.rfc-editor.org/rfc/rfc2046.html#section-5.1.3 RFC 2046 §5.1.3.>
  -- Independent body parts, bundled in a particular order.
  | Alternative
  -- ^ <https://www.rfc-editor.org/rfc/rfc2046.html#section-5.1.4 RFC 2046 §5.1.4.>
  -- Each part is an alternative version of the same content
  -- (e.g. plain text and HTML), in order of increasing faithfulness
  -- to the original content.
  | Digest
  -- ^ <https://www.rfc-editor.org/rfc/rfc2046.html#section-5.1.5 RFC 2046 §5.1.5.>
  -- Collection of messages. Parts should have @Content-Type: message/rfc822@.
  | Parallel
  -- ^ <https://www.rfc-editor.org/rfc/rfc2046.html#section-5.1.6 RFC 2046 §5.1.6.>
  -- Independent body parts, order not significants.  Parts may be
  -- displayed in parallel if the system supports it.
  | Related
      ContentType {- ^ @type@ -}
      (Maybe B.ByteString) {- ^ @start@ -}
      (Maybe B.ByteString) {- ^ @start-info@ -}
  -- ^ <https://www.rfc-editor.org/rfc/rfc2387.html RFC 2387.>
  -- Aggregate or compound objects.
  | Signed B.ByteString {- ^ protocol -} B.ByteString {- ^ micalg -}
  -- ^ <https://www.rfc-editor.org/rfc/rfc1847.html#section-2.1 RFC 1847 §2.1.>
  -- Signed messages.
  | Encrypted B.ByteString {- protocol -}
  -- ^ <https://www.rfc-editor.org/rfc/rfc1847.html#section-2.2 RFC 1847 §2.2.>
  | Report B.ByteString {- report-type -}
  -- ^ <https://www.rfc-editor.org/rfc/rfc6522.html RFC 6522>.
  -- Electronic mail reports.
  | Multilingual
  -- ^ <https://www.rfc-editor.org/rfc/rfc8255.html RFC 8255>.
  -- Multilingual messages.  The first part should be a multilingual
  -- explanatory preface.  Subsequent parts MUST have a
  -- @Content-Language@ and a @Content-Type@ field, and MAY have a
  -- @Content-Translation-Type@ field.
  | Unrecognised (CI B.ByteString)
  deriving (Eq, Show)

-- | MIME message body.  Either a single @Part@, or @Multipart@.
-- Only the body is represented; preamble and epilogue are not.
--
data MIME
  = Part B.ByteString
  | Encapsulated MIMEMessage
  | Multipart MultipartSubtype Boundary (NonEmpty MIMEMessage)
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
  Multipart sub b bs ->
    Message h . Multipart sub b <$> sequenceA (entities f <$> bs)
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

-- | @multipart/...; boundary=asdf@
contentTypeMultipart :: MultipartSubtype -> Boundary -> ContentType
contentTypeMultipart subtype boundary =
  ContentType "multipart" sub mempty
    & setParam "boundary" (unBoundary boundary)
    & appendParams
  where
    setParam k v = set (parameter k) (Just $ ParameterValue Nothing Nothing v)
    (sub, appendParams) = case subtype of
      Mixed -> ("mixed", id)
      Alternative -> ("alternative", id)
      Digest -> ("digest", id)
      Parallel -> ("parallel", id)
      Multilingual -> ("multilingual", id)
      Report typ -> ("report", setParam "report-type" typ)
      Signed proto micalg -> ("signed", setParam "protocol" proto . setParam "micalg" micalg)
      Encrypted proto -> ("encrypted", setParam "protocol" proto)
      Related typ start startInfo ->
        ( "related"
        , maybe id (setParam "start") start
          . maybe id (setParam "start-info") startInfo
          . setParam "type" (renderContentType typ)
        )
      Unrecognised sub' -> (sub', id)

-- | @multipart/mixed; boundary=asdf@
contentTypeMultipartMixed :: Boundary -> ContentType
contentTypeMultipartMixed = contentTypeMultipart Mixed

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
-- __Note__: when dealing with 'Multipart' or 'Encapsulated'
-- messages, the @Content-Type@ header will be overridden when
-- serialising the message.  This avoids scenarios where the
-- @Content-Type@ does not match the structure of the message.  In
-- general, the @Content-Type@ header should be treated as "read
-- only" for multipart or encapsulated message.
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
{-# ANN dispositionType ("HLint: ignore Avoid lambda using `infix`" :: String) #-}

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
  (>>= either (const Nothing) Just . Data.IMF.parse parseContentDisposition)
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


-- | Traversal of @boundary@ parameter (which may be unspecified)
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
        case prepMultipart ct of
          Left err              -> FailedParse err <$> takeTillEnd
          Right (sub, boundary) ->
            Multipart sub boundary <$> multipart takeTillEnd boundary
            <|> FailedParse MultipartParseFail <$> takeTillEnd
     | matchContentType "message" (Just "rfc822") ct ->
        (Encapsulated <$> message (mime' takeTillEnd))
        <|> (FailedParse EncapsulatedMessageParseFail <$> takeTillEnd)
  _ -> Part <$> takeTillEnd
  where
    prepMultipart ct =
      (,) <$> parseSubtype ct <*> parseBoundary ct
    parseBoundary ct =
      getRequiredParam "boundary" ct
      >>= over _Left (InvalidParameterValue "boundary") . makeBoundary
    getRequiredParam k =
      maybe (Left $ RequiredParameterMissing k) Right . preview (rawParameter k)
    getOptionalParam k =
      Right . preview (rawParameter k)
    parseSubtype ct = case view ctSubtype ct of
      "mixed"         -> pure Mixed
      "alternative"   -> pure Alternative
      "digest"        -> pure Digest
      "parallel"      -> pure Parallel
      "multilingual"  -> pure Multilingual
      "report"        -> Report <$> getRequiredParam "report-type" ct
      "signed"        -> Signed
                          <$> getRequiredParam "protocol" ct
                          <*> getRequiredParam "micalg" ct
      "encrypted"     -> Encrypted <$> getRequiredParam "protocol" ct
      "related"       -> Related
                          <$> ( getRequiredParam "type" ct
                              >>= \s -> maybe (Left $ InvalidParameterValue "type" s) Right
                                          (preview (parsed parseContentType) s)
                              )
                          <*> getOptionalParam "start" ct
                          <*> getOptionalParam "start-info" ct
      unrecognised    -> pure $ Unrecognised unrecognised

data MIMEParseError
  = RequiredParameterMissing (CI B.ByteString)
  | InvalidParameterValue (CI B.ByteString) B.ByteString
  | MultipartParseFail
  | EncapsulatedMessageParseFail
  deriving (Eq, Show)

-- | Parse a multipart MIME message.  Preambles and epilogues are
-- discarded.
--
multipart
  :: Parser B.ByteString  -- ^ parser to the end of the part
  -> Boundary             -- ^ boundary, sans leading "--"
  -> Parser (NonEmpty MIMEMessage)
multipart takeTillEnd boundary =
  skipTillString dashBoundary *> crlf -- FIXME transport-padding
  *> fmap fromList (part `sepBy1` crlf)
  <* string "--" <* takeTillEnd
  where
    delimiter = "\n--" <> unBoundary boundary
    dashBoundary = B.tail delimiter
    part = message (mime' (trim <$> takeTillString delimiter))
    trim s  -- trim trailing CR, because we only searched for LF
      | B.null s = s
      | C8.last s == '\r' = B.init s
      | otherwise = s

-- | Sets the @MIME-Version: 1.0@ header.
--
instance RenderMessage MIME where
  tweakHeaders b h =
    h
    & set (headers . at "MIME-Version") (Just "1.0")
    & setContentType
    where
      setContentType = case b of
        Multipart sub boundary _  -> set contentType (contentTypeMultipart sub boundary)
        Encapsulated _msg         -> set contentType "message/rfc822"
        _                         -> id
  buildBody _h z = Just $ case z of
    Part partbody -> Builder.byteString partbody
    Encapsulated msg -> buildMessage msg
    Multipart _sub b xs ->
      let
        boundary = "--" <> Builder.byteString (unBoundary b)
      in
        boundary <> "\r\n"
        <> fold (intersperse ("\r\n" <> boundary <> "\r\n") (fmap buildMessage xs))
        <> "\r\n" <> boundary <> "--\r\n"
    FailedParse _ bs -> Builder.byteString bs

-- | Create a mixed `MIMEMessage` with an inline text/plain part and multiple
-- `attachments`
--
createMultipartMixedMessage
    :: Boundary
    -> NonEmpty MIMEMessage -- ^ parts
    -> MIMEMessage
createMultipartMixedMessage b attachments' =
  let hdrs = Headers [] & set contentType (contentTypeMultipartMixed b)
  in Message hdrs (Multipart Mixed b attachments')

-- | Create an inline, text/plain, utf-8 encoded message
--
createTextPlainMessage :: T.Text -> MIMEMessage
createTextPlainMessage s = setTextPlainBody s (Message (Headers []) ())

-- | Set an inline, @text/plain@, utf-8 encoded message body
--
setTextPlainBody :: T.Text -> Message ctx a -> MIMEMessage
setTextPlainBody s =
  fmap Part
  . transferEncode
  . charsetEncode
  . set contentDisposition (Just $ ContentDisposition Inline mempty)
  . set contentType contentTypeTextPlain
  . set body s

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
  hdrs = Headers []
          & set contentType ct
          & set contentDisposition (Just cd)

-- | Encapsulate a message as a @message/rfc822@ message.
-- You can use this in creating /forwarded/ or /bounce/ messages.
--
encapsulate :: MIMEMessage -> MIMEMessage
encapsulate = Message hdrs . Encapsulated
  where
  hdrs = Headers [] & set contentType "message/rfc822"
