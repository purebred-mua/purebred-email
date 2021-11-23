-- This file is part of purebred-email
-- Copyright (C) 2017-2021  Fraser Tweedale and Róman Joost
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

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{- |

Internet Message Format (IMF), which is used for electronic mail
(email), as specified by
<https://tools.ietf.org/html/rfc5322 RFC 5322> and updated by
<https://tools.ietf.org/html/rfc6854 RFC 6854>.

The parser allows LF line endings in addition to CRLF.  RFC 5322
specifies CRLF line endings but LF-only is common in on-disk
formats.  Serialisation functions produces CRLF line endings.

The main parsing function is 'message'.  It takes a second function
that can inspect the headers to determine how to parse the body.

@
'message' :: ('Headers' -> 'BodyHandler' a) -> Parser ('Message' ctx a)
@

The 'Message' type is parameterised over the body type, and a
phantom type that can be used for context.

@
data 'Message' ctx a = 'Message' 'Headers' a
@

Headers and body can be accessed via the 'headers', 'header' and
'body' optics.

@
'headers' :: 'HasHeaders' a => Lens'       a         Headers
headers ::                 Lens' ('Message' ctx b) Headers

'header' :: 'HasHeaders' a => CI B.ByteString -> Traversal'        a        B.ByteString
header ::                 CI B.ByteString -> Traversal' ('Message' ctx b) B.ByteString
header ::                 CI B.ByteString -> Traversal'     'Headers'     B.ByteString

'body' :: Lens ('Message' ctx a) (Message ctx' b) a b
@

The following example program parses an input, interpreting the body
as a raw @ByteString@, and prints the subject (if present), the
number of headers and the body length.  The message context type is
@()@.

@
analyse :: B.ByteString -> IO ()
analyse input =
  case 'parse' ('message' (const takeByteString)) of
    Left errMsg -> hPutStrLn stderr errMsg *> exitFailure
    Right (msg :: Message () B.ByteString) -> do
      T.putStrLn $ "subject: " <> foldOf ('headerSubject' 'defaultCharsets') msg
      putStrLn $ "num headers: " <> show (length (view 'headers' msg))
      putStrLn $ "body length: " <> show (B.length (view 'body' msg))
@

-}
module Data.IMF
  (
  -- * Message types
    Message(..)
  , message
  , MessageContext
  , BodyHandler(..)
  , body
  , EqMessage(..)

  -- * Replying
  , reply
  , ReplySettings(ReplySettings)
  , defaultReplySettings
  , ReplyMode(..)
  , ReplyFromMode(..)
  , ReplyFromRewriteMode(..)
  , SelfInRecipientsMode(..)
  , AuthorMailboxes
  , replyMode
  , replyFromMode
  , replyFromRewriteMode
  , selfInRecipientsMode
  , authorMailboxes

  -- * Headers
  , Header
  , HasHeaders(..)
  , headerList
  , Headers(..)

  -- ** Date and Time
  , headerDate
  , dateTime

  -- ** Originator
  , headerFrom
  , headerReplyTo

  -- ** Destination Address
  , headerTo
  , headerCC
  , headerBCC

  -- ** Identification
  , headerMessageID
  , headerInReplyTo
  , headerReferences

  -- ** Informational
  , headerSubject

  -- ** Arbitrary headers
  , header
  , headerText

  -- * Types

  -- ** Message ID
  , MessageID
  , parseMessageID
  , buildMessageID
  , renderMessageID

  -- ** Address types
  , Address(..)
  , address
  , addressList
  , AddrSpec(..)
  , Domain(..)
  , Mailbox(..)
  , mailbox
  , mailboxList

  -- * Parsers
  , parse
  , parsed
  , parsePrint
  , crlf
  , quotedString

  -- * Helpers
  , field

  -- * Serialisation
  , buildMessage
  , renderMessage
  , RenderMessage(..)
  , renderRFC5322Date
  , buildFields
  , buildField
  , renderAddressSpec
  , renderMailbox
  , renderMailboxes
  , renderAddress
  , renderAddresses
  ) where

import Control.Applicative
import Data.Either (fromRight)
import Data.Foldable (fold, toList)
import Data.Function (on)
import Data.List (find, findIndex, intersperse)
import Data.List.NonEmpty (NonEmpty, head, intersperse)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Monoid (First(..))
import Data.String (IsString(..))
import Data.Word (Word8)
import GHC.Generics (Generic)

import Control.DeepSeq (NFData)
import Control.Lens
import Control.Lens.Cons.Extras (recons)
import Data.Attoparsec.ByteString as A hiding (parse, take)
import Data.Attoparsec.ByteString.Char8 (char8)
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Prim as Prim
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (ZonedTime, defaultTimeLocale, formatTime)

import Data.IMF.Syntax
  ( CI, ci, mk, original
  , (<<>>), foldMany, foldMany1Sep
  , char, fromChar, isAtext, isQtext, isVchar, isWsp
  , optionalCFWS, word, wsp, vchar, optionalFWS, crlf
  , domainLiteral, dotAtom, dotAtomText, localPart, quotedString
  )
import {-# SOURCE #-} Data.IMF.Text (readMailbox)
import Data.IMF.DateTime (dateTime)
import Data.MIME.Charset
import Data.MIME.EncodedWord
import Data.MIME.TransferEncoding (transferEncode)

type Header = (CI B.ByteString, B.ByteString)
newtype Headers = Headers [Header]
  deriving (Eq, Show, Generic, NFData)

class HasHeaders a where
  headers :: Lens' a Headers

instance HasHeaders Headers where
  headers = id

type instance Index Headers = CI B.ByteString
type instance IxValue Headers = B.ByteString

instance Ixed Headers where
  ix = header

hdriso :: Iso' Headers [(CI B.ByteString, B.ByteString)]
hdriso = iso (\(Headers xs) -> xs) Headers

-- | Acts upon the first occurrence of the header only.
--
instance At Headers where
  at k = hdriso . l
    where
    l :: Lens' [(CI B.ByteString, B.ByteString)] (Maybe B.ByteString)
    l f kv =
      let
        i = findIndex ((== k) . fst) kv
        g Nothing = maybe kv (\j -> take j kv <> drop (j + 1) kv) i
        g (Just v) = maybe ((k,v):kv) (\j -> set (ix j) (k,v) kv) i
      in
        g <$> f (lookup k kv)


-- | Target all values of the given header
header :: HasHeaders a => CI B.ByteString -> Traversal' a B.ByteString
header k = headerList . traversed . filtered ((k ==) . fst) . _2

-- | Message type, parameterised over context and body type.  The
-- context type is not used in this module but is provided for uses
-- such as tracking the transfer/charset encoding state in MIME
-- messages.
--
data Message s a = Message Headers a
  deriving (Show, Generic, NFData)

instance HasHeaders (Message s a) where
  headers f (Message h b) = fmap (`Message` b) (f h)

instance Functor (Message s) where
  fmap f (Message h a) = Message h (f a)

-- | How to compare messages with this body type.
--
-- This class arises because we may want to tweak the headers,
-- possibly in response to body data, or vice-versa, when
-- comparing messages.
--
-- The default implementation compares headers and body using (==).
--
class EqMessage a where
  eqMessage :: Message s a -> Message s a -> Bool

  default eqMessage :: (Eq a) => Message s a -> Message s a -> Bool
  eqMessage (Message h1 b1) (Message h2 b2) = h1 == h2 && b1 == b2


instance EqMessage a => Eq (Message s a) where
  (==) = eqMessage

-- | Access headers as a list of key/value pairs.
headerList :: HasHeaders a => Lens' a [(CI B.ByteString, B.ByteString)]
headerList = headers . coerced

body :: Lens (Message ctx a) (Message ctx' b) a b
body f (Message h b) = fmap (\b' -> Message h b') (f b)
{-# ANN body ("HLint: ignore Avoid lambda" :: String) #-}


-- §3.3  Date and Time Specification
-- Sat, 29 Sep 2018 12:51:05 +1000
rfc5322DateTimeFormat :: String
rfc5322DateTimeFormat = "%a, %d %b %Y %T %z"

renderRFC5322Date :: ZonedTime -> B.ByteString
renderRFC5322Date = Char8.pack . formatTime defaultTimeLocale rfc5322DateTimeFormat

headerDate :: HasHeaders a => Lens' a (Maybe ZonedTime)
headerDate = headers . at "Date" . iso (>>= p) (fmap renderRFC5322Date)
  where
  p = either (const Nothing) Just . parseOnly (dateTime <* endOfInput)

-- §3.4 Address Specification
buildMailbox :: Mailbox -> Builder.Builder
buildMailbox (Mailbox n a) =
  maybe a' (\n' -> buildPhrase n' <> " <" <> a' <> ">") n
  where
    a' = buildAddressSpec a

-- Encode a phrase.
--
-- * Empty string is special case; must be in quotes
-- * If valid as an atom, use as-is (ideally, but we don't do this yet)
-- * If it can be in a quoted-string, do so.
-- * Otherwise make it an encoded-word
--
buildPhrase :: T.Text -> Builder.Builder
buildPhrase "" = "\"\""
buildPhrase s =
  case enc s of
    PhraseAtom -> T.encodeUtf8Builder s
    PhraseQuotedString -> qsBuilder False
    PhraseQuotedStringEscapeSpace -> qsBuilder True
    PhraseEncodedWord -> buildEncodedWord . transferEncode . charsetEncode $ s
  where
    enc = snd . T.foldr (\c (prev, req) -> (c, encChar prev c <> req)) ('\0', mempty)
    encChar prev c
      | isAtext c = PhraseAtom
      | isQtext c = PhraseQuotedString
      | isVchar c = PhraseQuotedString
      | c == ' ' =
          if prev == ' '  -- two spaces in a row; need to avoid FWS
          then PhraseQuotedStringEscapeSpace
          else PhraseQuotedString
      | otherwise = PhraseEncodedWord

    qsBuilder escSpace = "\"" <> T.encodeUtf8BuilderEscaped (escPrim escSpace) s <> "\""
    escPrim escSpace = Prim.condB (\c -> isQtext c || not escSpace && c == 32)
      (Prim.liftFixedToBounded Prim.word8)
      (Prim.liftFixedToBounded $ (fromChar '\\',) Prim.>$< Prim.word8 Prim.>*< Prim.word8)

-- | Data type used to compute escaping requirement of a Text 'phrase'
--
data PhraseEscapeRequirement
  = PhraseAtom
  | PhraseQuotedString
  | PhraseQuotedStringEscapeSpace
  | PhraseEncodedWord
  deriving (Eq, Ord)

instance Semigroup PhraseEscapeRequirement where
  PhraseEncodedWord <> _ =
    -- allows early termination of folds
    PhraseEncodedWord
  l <> r = max l r

instance Monoid PhraseEscapeRequirement where
  mempty = PhraseAtom



renderMailboxes :: [Mailbox] -> B.ByteString
renderMailboxes = L.toStrict . Builder.toLazyByteString . buildMailboxes

buildMailboxes :: [Mailbox] -> Builder.Builder
buildMailboxes = fold . Data.List.intersperse ", " . fmap buildMailbox

renderMailbox :: Mailbox -> B.ByteString
renderMailbox = L.toStrict . Builder.toLazyByteString . buildMailbox

mailbox :: CharsetLookup -> Parser Mailbox
mailbox charsets =
  Mailbox <$> optional (displayName charsets) <*> angleAddr
  <|> Mailbox Nothing <$> addressSpec

phrase :: CharsetLookup -> Parser T.Text
phrase charsets = foldMany1Sep " " $
  -- RFC 2047 §2: if it is desirable to encode more text than will
  -- fit in an 'encoded-word' of 75 characters, multiple
  -- 'encoded-word's (separated by CRLF SPACE) may be used.
  --
  -- The initial header parsing unfolds the header, so such
  -- "continuation" encoded-words are now separated by SPACE.  The
  -- CRLFs have been erased.  Naïvely, this seems to make this case
  -- indistinguishable from "consecutive" encoded-words that were
  -- actually separated by SPACE.  However, a careful examination of
  -- the grammar shows that encoded-words in a 'phrase' cannot be
  -- separated by whitespace:
  --
  -- @
  -- phrase         = 1*( encoded-word / word )
  -- encoded-word   = "=?" charset "?" encoding "?" encoded-text "?="
  -- word           = atom / quoted-string
  -- atom           = [CFWS] 1*atext [CFWS]
  -- quoted-string  = [CFWS]
  --                  DQUOTE *([FWS] qcontent) [FWS] DQUOTE
  --                  [CFWS]
  -- @
  --
  -- The only place whitespace is allowed is within 'atom' and
  -- 'quoted-string'.  Therefore two encoded-words separated by
  -- SPACE must be the result of folding a long encoded-word.  So
  -- consume as many SPACE separated encoded-words as possible,
  -- decode them, and concatenate the result.
  fmap
    ( foldMap (decodeEncodedWord charsets) )
    ( ("=?" *> encodedWord) `sepBy1` char8 ' ' )
  <|> fmap decodeLenient word

displayName :: CharsetLookup -> Parser T.Text
displayName = phrase

angleAddr :: Parser AddrSpec
angleAddr = optionalCFWS *>
  char8 '<' *> addressSpec <* char8 '>'
  <* optionalCFWS

buildAddressSpec :: AddrSpec -> Builder.Builder
buildAddressSpec (AddrSpec lp (DomainDotAtom b))
  | " " `B.isInfixOf` lp = "\"" <> buildLP <> "\"" <> rest
  | otherwise = buildLP <> rest
  where
    buildLP = Builder.byteString lp
    rest = "@" <> foldMap (Builder.byteString . original)
                          (Data.List.NonEmpty.intersperse "." b)
buildAddressSpec (AddrSpec lp (DomainLiteral b)) =
  foldMap Builder.byteString [lp, "@", b]

renderAddressSpec :: AddrSpec -> B.ByteString
renderAddressSpec = L.toStrict . Builder.toLazyByteString . buildAddressSpec

addressSpec :: Parser AddrSpec
addressSpec = AddrSpec <$> localPart <*> (char8 '@' *> domain)

-- | Printable US-ASCII excl "[", "]", or "\"
isDtext :: Word8 -> Bool
isDtext c = (c >= 33 && c <= 90) || (c >= 94 && c <= 126)

domain :: Parser Domain
domain = (DomainDotAtom . fmap mk <$> dotAtom)
         <|> (DomainLiteral <$> domainLiteral)

mailboxList :: CharsetLookup -> Parser [Mailbox]
mailboxList charsets = mailbox charsets `sepBy` char8 ','

renderAddresses :: [Address] -> B.ByteString
renderAddresses xs = B.intercalate ", " $ renderAddress <$> xs

renderAddress :: Address -> B.ByteString
renderAddress (Single m) = renderMailbox m
renderAddress (Group name xs) = T.encodeUtf8 name <> ":" <> renderMailboxes xs <> ";"

addressList :: CharsetLookup -> Parser [Address]
addressList charsets = address charsets `sepBy` char8 ','

group :: CharsetLookup -> Parser Address
group charsets =
  Group <$> displayName charsets <* char8 ':'
        <*> mailboxList charsets <* char8 ';' <* optionalCFWS

address :: CharsetLookup -> Parser Address
address charsets =
  group charsets <|> Single <$> mailbox charsets

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

-- | Map a single-occurrence header to a Maybe value.
-- On read, absent header or parse failure maps to Nothing.
-- On write, Nothing results in absent header.
headerSingleToMaybe
  :: (HasHeaders s)
  => (B.ByteString -> Maybe a)
  -> (a -> B.ByteString)
  -> CI B.ByteString
  -> Lens' s (Maybe a)
headerSingleToMaybe f g k = headers . at k . iso (>>= f) (fmap g)

headerAddressList :: (HasHeaders a) => CI B.ByteString -> CharsetLookup -> Lens' a [Address]
headerAddressList k charsets = headerSingleToList
  (fromRight [] . parseOnly (addressList charsets))
  renderAddresses
  k

headerFrom, headerReplyTo, headerTo, headerCC, headerBCC
  :: (HasHeaders a)
  => CharsetLookup -> Lens' a [Address]
headerFrom = headerAddressList "From"
headerReplyTo = headerAddressList "Reply-To"
headerTo = headerAddressList "To"
headerCC = headerAddressList "Cc"
headerBCC = headerAddressList "Bcc"

data MessageID = MessageID
  (NonEmpty B.ByteString)
  (Either (NonEmpty B.ByteString) B.ByteString)
  deriving (Eq, Ord)

instance Show MessageID where
  show = Char8.unpack . renderMessageID

parseMessageID :: Parser MessageID
parseMessageID =
  MessageID
    <$> (optionalCFWS *> char '<'  *> idLeft) <* char '@'
    <*> idRight <* char '>' <* optionalCFWS
  where
  idLeft = dotAtomText
  idRight = Left <$> dotAtomText <|> Right <$> noFoldLiteral
  noFoldLiteral = char '[' *> A.takeWhile1 isDtext <* char ']'

buildMessageID :: MessageID -> Builder.Builder
buildMessageID (MessageID l r) =
  "<" <> buildDotAtom l <> "@" <> either buildDotAtom buildNoFoldLit r <> ">"
  where
  buildDotAtom =
    fold . Data.List.NonEmpty.intersperse "." . fmap Builder.byteString
  buildNoFoldLit s =
    "[" <> Builder.byteString s <> "]"

renderMessageID :: MessageID -> B.ByteString
renderMessageID = L.toStrict . Builder.toLazyByteString . buildMessageID

headerMessageID :: (HasHeaders a) => Lens' a (Maybe MessageID)
headerMessageID = headerSingleToMaybe
  (either (const Nothing) Just . parseOnly (parseMessageID <* endOfInput))
  renderMessageID
  "Message-ID"

headerMessageIDList :: (HasHeaders a) => CI B.ByteString -> Lens' a [MessageID]
headerMessageIDList = headerSingleToList
  (fromRight [] . parseOnly (many parseMessageID <* endOfInput))
  ( L.toStrict . Builder.toLazyByteString
    . fold . Data.List.intersperse " " . fmap buildMessageID )

headerInReplyTo, headerReferences :: (HasHeaders a) => Lens' a [MessageID]
headerInReplyTo = headerMessageIDList "In-Reply-To"
headerReferences = headerMessageIDList "References"


-- | Single-valued header with @Text@ value via encoded-words.
-- The conversion to/from Text is total (encoded-words that failed to be
-- decoded are passed through unchanged).  Therefore @Nothing@ means that
-- the header was not present.
--
headerText :: (HasHeaders a) => CharsetLookup -> CI B.ByteString -> Lens' a (Maybe T.Text)
headerText charsets k =
  headers . at k . iso (fmap (decodeEncodedWords charsets)) (fmap encodeEncodedWords)

-- | Subject header.  See 'headerText' for details of conversion to @Text@.
headerSubject :: (HasHeaders a) => CharsetLookup -> Lens' a (Maybe T.Text)
headerSubject charsets = headerText charsets "Subject"


{- Replying -}

-- | Specify how to choose recipients when replying.
--
-- __TODO__: "list reply" mode
--
data ReplyMode
  = ReplyToSender
  -- ^ Reply to the sender of the email only, or @Reply-To@ header if set.
  | ReplyToGroup
  -- ^ Reply to sender and @Cc@ all other recipients of the original message.

-- | The mailboxes of the entity authoring the reply.
-- The first mailbox is the "preferred" mailbox.
type AuthorMailboxes = NonEmpty Mailbox

-- | How to choose the @From@ address.
data ReplyFromMode
  = ReplyFromPreferredMailbox
  -- ^ Always reply @From@ the preferred mailbox
  | ReplyFromMatchingMailbox
  -- ^ Reply from whichever author mailbox is a recipient of the
  -- parent message, or the preferred mailbox if none of the author
  -- mailboxes is a visible recipient of the parent message.

-- | Whether to use the @From@ address as it appears in the parent
-- message, or as it appears in the 'AuthorMailboxes'.
--
data ReplyFromRewriteMode
  = ReplyFromRewriteOff
  -- ^ Use the @From@ mailbox as it appears in the original message.
  | ReplyFromRewriteOn
  -- ^ Use the @From@ mailbox as it appears in the author mailboxes.

data SelfInRecipientsMode
  = SelfInRecipientsRemove
  -- ^ Remove author mailbox from list of recipients when replying.
  | SelfInRecipientsIgnore
  -- ^ If author mailbox appears in list of recipients, leave it there.

-- | All the settings to control how to construct a reply to a message.
data ReplySettings = ReplySettings
  { _replyMode            :: ReplyMode
  , _replyFromMode        :: ReplyFromMode
  , _replyFromRewriteMode :: ReplyFromRewriteMode
  , _selfInRecipientsMode :: SelfInRecipientsMode
  , _authorMailboxes      :: AuthorMailboxes
  }

-- | Given author mailboxes, get a default 'ReplySettings'.  The default
-- settings are: 'ReplyToSender', 'ReplyFromMatchingMailbox',
-- 'ReplyFromRewriteOn', and 'SelfInRecipientsRemove'.
--
defaultReplySettings :: AuthorMailboxes -> ReplySettings
defaultReplySettings = ReplySettings
  ReplyToSender
  ReplyFromMatchingMailbox
  ReplyFromRewriteOn
  SelfInRecipientsRemove

replyMode :: Lens' ReplySettings ReplyMode
replyMode = lens _replyMode (\s a -> s { _replyMode = a })

replyFromMode :: Lens' ReplySettings ReplyFromMode
replyFromMode = lens _replyFromMode (\s a -> s { _replyFromMode = a })

replyFromRewriteMode :: Lens' ReplySettings ReplyFromRewriteMode
replyFromRewriteMode =
  lens _replyFromRewriteMode (\s a -> s { _replyFromRewriteMode = a })

selfInRecipientsMode :: Lens' ReplySettings SelfInRecipientsMode
selfInRecipientsMode =
  lens _selfInRecipientsMode (\s a -> s { _selfInRecipientsMode = a })

authorMailboxes :: Lens' ReplySettings AuthorMailboxes
authorMailboxes = lens _authorMailboxes (\s a -> s { _authorMailboxes = a })


replyRecipients
  :: CharsetLookup -> ReplySettings -> Message ctx a -> ([Address], [Address])
replyRecipients charsets settings msg =
  let
    mode = view replyMode settings
    rt = view (headerReplyTo charsets) msg
    f = view (headerFrom charsets) msg
    t = view (headerTo charsets) msg
    c = view (headerCC charsets) msg
  in case mode of
    ReplyToSender
      | not (null rt) -> (rt, [])
      | otherwise     -> (f, [])
    ReplyToGroup
      | length (t <> c) <= 1
      -> replyRecipients charsets (set replyMode ReplyToSender settings) msg
      | otherwise
      -> (f, t <> c)

replyReferences :: Message ctx a -> [MessageID]
replyReferences msg
  | null refer, length inRep == 1 = inRep <> msgId
  | otherwise                     = refer <> msgId
  where
  msgId = toList $ view headerMessageID msg
  refer = view headerReferences msg
  inRep = view headerInReplyTo msg

replySubject :: CharsetLookup -> Message ctx a -> T.Text
replySubject charsets msg = if prefixed then orig else "Re: " <> orig
  where
  orig = fold $ view (headerSubject charsets) msg
  prefixed = mk (T.take 3 orig) == "Re:"


-- | Construct a reply to a 'Message', according to the specified
-- 'ReplySettings' and following the requirements and suggestions of
-- RFC 5322.  In particular:
--
-- * Sets @In-Reply-To@ to the @Message-ID@ of the parent message.
--
-- * Sets the @References@ header, following the requirements in RFC
-- 5322 §3.6.4.
--
-- * Sets the @Subject@ by prepending @"Re: "@ to the parent
-- subject, unless it already has such a prefix (case-insensitive
-- match).  This is the scheme suggested in RFC 5322 §3.6.5.
--
-- * Sets the @From@ header.  If the 'ReplyFromMode' is
-- 'ReplyFromMatchingMailbox' and one of the 'authorMailboxes' is a
-- recipient of the parent message, that address will be used as the
-- @From@ address.  Also, if 'ReplyFromRewriteMode' is
-- 'ReplyFromRewriteOn', the matching value in 'authorMailboxes'
-- replaces the value from the parent message.  This can be used to
-- rewrite a bare address to one with a display name (or
-- vice-versa).  In all other cases the @From@ address will be the
-- /preferred/ (first) author mailbox.
--
-- * Sets @To@ and @Cc@ according to 'ReplyMode' and
-- 'SelfInRecipientsMode'.  These headers are described in RFC 5322
-- §3.6.3.
--
--     * In 'ReplyToSender' mode, the @To@ header of the reply will
--     contain the addresses from the @Reply-To@ header if it is
--     present, otherwise it will contain the addresses from the
--     @From@ header.
--
--     * In 'ReplyToGroup' mode, if the parent message has only one
--     recipient (across the @To@ and @Cc@ headers), the behaviour
--     is the same as 'ReplyToSender' mode (@Reply-To@ is respected).
--     If the parent message has multiple recipients, the
--     @Reply-To@ header is ignored, the @To@ header of the reply
--     will contain the addresses from the @From@ header, and the
--     @Cc@ header of the reply will contain the addresses from the
--     @To@ and @Cc@ headers.
--
--     * If the 'SelfInRecipientsMode' is 'SelfInRecipientsRemove',
--     any of the 'authorMailboxes' will be removed from the @To@
--     and @Cc@ headers.
--
reply
  :: CharsetLookup
  -> ReplySettings
  -> Message ctx a
  -> Message ctx ()
reply charsets settings msg =
  let
    self = view authorMailboxes settings

    getAddrSpec :: Mailbox -> AddrSpec
    getAddrSpec (Mailbox _ addr) = addr

    -- | Find a mailbox matching the given address.  If no match is
    -- found, return @Nothing@.  If match is found, return the value
    -- from the candidates collection if 'ReplyFromRewriteOn',
    -- otherwise return the input value.
    findMatchingMailbox
      :: (Foldable t)
      => t Mailbox -> Address -> Maybe Mailbox
    findMatchingMailbox xs (Single addr) =
      f <$> find (on (==) getAddrSpec addr) xs
      where
        f = case view replyFromRewriteMode settings of
          ReplyFromRewriteOn  -> id
          ReplyFromRewriteOff -> const addr
    findMatchingMailbox _ _ = Nothing

    getSelf :: Address -> Maybe Mailbox
    getSelf = findMatchingMailbox self

    isSelf :: Address -> Bool
    isSelf = isJust . getSelf

    findSelf =
      let
        parentTo = view (headerTo charsets) msg
        parentCc = view (headerCC charsets) msg
      in
        getFirst $ foldMap (First . getSelf) (parentTo <> parentCc)

    filterSelf = case view selfInRecipientsMode settings of
      SelfInRecipientsRemove -> filter (not . isSelf)
      SelfInRecipientsIgnore -> id

    (t, c) = replyRecipients charsets settings msg
    _To = filterSelf t
    _To_mailboxes = mapMaybe (\case Single a -> Just a ; _ -> Nothing) _To
    _Cc = c
          & filterSelf
          & filter (isNothing . findMatchingMailbox _To_mailboxes)
    _From =
      let preferred = Data.List.NonEmpty.head self
      in
        case view replyFromMode settings of
          ReplyFromPreferredMailbox -> preferred
          ReplyFromMatchingMailbox  -> fromMaybe preferred findSelf

    hdrs = Headers []
      & set (headerFrom charsets) [Single _From]
      & set (headerTo charsets) (filterSelf _To)
      & set (headerCC charsets) (filterSelf _Cc)
      & set headerInReplyTo (toList $ view headerMessageID msg)
      & set headerReferences (replyReferences msg)
      & set (headerSubject charsets) (Just $ replySubject charsets msg)
  in
    Message hdrs ()


-- §3.5.  Overall Message Syntax


-- | Specify how to handle a message body, including the possibility
-- of optional bodies and no body (which is distinct from empty body).
data BodyHandler a
  = RequiredBody (Parser a)
  | OptionalBody (Parser a, a)
  -- ^ If body is present run parser, otherwise use constant value
  | NoBody a

-- | Parse a message.  The function argument receives the headers and
-- yields a handler for the message body.
--
message :: (Headers -> BodyHandler a) -> Parser (Message (MessageContext a) a)
message f = fields >>= \hdrs -> Message hdrs <$> case f hdrs of
  RequiredBody b -> crlf *> b
  OptionalBody (b, a) -> optional crlf >>= maybe (pure a) (const b)
  NoBody b -> pure b

type family MessageContext a


fields :: Parser Headers
fields = Headers <$> many field

-- | Define how to render an RFC 5322 message with given payload type.
--
class RenderMessage a where
  -- | Build the body.  If there should be no body (as distinct from
  -- /empty body/) return Nothing
  buildBody :: Headers -> a -> Maybe Builder.Builder

  -- | Allows tweaking the headers before rendering.  Default
  -- implementation is a no-op.
  tweakHeaders :: a -> Headers -> Headers
  tweakHeaders _ = id

-- | Construct a 'Builder.Builder' for the message.  This allows efficient
-- streaming to IO handles.
--
buildMessage :: forall ctx a. (RenderMessage a) => Message ctx a -> Builder.Builder
buildMessage (Message h b) =
  buildFields (tweakHeaders b h)
  <> maybe mempty ("\r\n" <>) (buildBody h b)

-- | Render a message to a lazy 'L.ByteString'.  (You will probably not
-- need a strict @ByteString@ and it is inefficient for most use cases.)
--
renderMessage :: (RenderMessage a) => Message ctx a -> L.ByteString
renderMessage = Builder.toLazyByteString . buildMessage

-- Header serialisation
buildFields :: Headers -> Builder.Builder
buildFields = foldMapOf (hdriso . traversed) buildField

buildField :: (CI B.ByteString, B.ByteString) -> Builder.Builder
buildField (k,v) =
  let key = original k
  in
    Builder.byteString key
    <> ":"
    <> foldUnstructured v (B.length key + 1)
    <> "\r\n"

-- | Render a field body with proper folding
--
-- Folds on whitespace (and only whitespace).  Sequential whitespace
-- chars are folded.  That's OK because the grammar says it is
-- folding whitespace.
--
foldUnstructured :: B.ByteString -> Int -> Builder.Builder
foldUnstructured s i = case Char8.words s of
  [] -> mempty
  (h:t) ->
    -- Special case to prevent wrapping of first word;
    -- see 6dbc04fb1863e845699b1cef50f4edaf1326bdae for info.
    " " <> Builder.byteString h <> go t (i + 1 + B.length h)
  where
  limit = 76  -- could be 78, but this preserves old behaviour
  go [] _ = mempty
  go (chunk:chunks) col
    | col <= 1 || col + 1 + B.length chunk < limit =
        -- either there is room for the chunk, or we are at the
        -- beginning of a line so add it here anyway (otherwise
        -- we will add "\r\n" and recurse forever)
        " " <> Builder.byteString chunk <> go chunks (col + 1 + B.length chunk)
    | otherwise = "\r\n" <> go (chunk:chunks) 0  -- fold

-- | Printable ASCII excl. ':'
isFtext :: Word8 -> Bool
isFtext c = (c >= 33 && c <= 57) || (c >= 59 && c <= 126)

field :: Parser (CI B.ByteString, B.ByteString)
field = (,)
  <$> ci (takeWhile1 isFtext)
  <*  char8 ':' <* many wsp
  <*> unstructured <* crlf

unstructured :: Parser B.ByteString
unstructured =
  foldMany (optionalFWS <<>> (B.singleton <$> vchar))
  <<>> A.takeWhile isWsp


-- | Given a parser, construct a 'Fold'
--
-- See 'parse' for discussion of performance.
--
parsed :: (Cons s s Word8 Word8) => Parser a -> Fold s a
parsed p = to (parse p) . folded
{-# INLINE parsed #-}

-- | Construct a prism from a parser and a printer
parsePrint :: Parser a -> (a -> B.ByteString) -> Prism' B.ByteString a
parsePrint fwd rev = prism' rev (AL.maybeResult . AL.parse fwd . view recons)

-- | Parse an @a@.
--
-- Converts the input to a /lazy/ @ByteString@.
-- Build with rewrite rules enabled (@-O@, cabal's default)
-- to achieve the following conversion overheads:
--
-- * Lazy @ByteString@: no conversion
-- * Strict @ByteString@: /O(1)/ conversion
-- * @[Word8]@: /O(n)/ conversion
--
-- It is __recommended to use strict bytestring__ input.  Parsing a
-- lazy bytestring will cause numerous parser buffer resizes.  The
-- lazy chunks in the input can be GC'd but the buffer keeps growing
-- so you don't actually keep the memory usage low by using a lazy
-- bytestring.
--
parse :: (Cons s s Word8 Word8) => Parser a -> s -> Either String a
parse p = AL.eitherResult . AL.parse p . view recons
{-# INLINE parse #-}


-- | Email address with optional display name.
-- The @Eq@ instance compares the display name case
-- sensitively and the address as described at 'AddrSpec'.
--
data Mailbox =
    Mailbox (Maybe T.Text {- display name -})
             AddrSpec
    deriving (Show, Eq, Generic, NFData)

instance IsString Mailbox where
  fromString =
    either (error . mappend "Failed to parse Mailbox: ") id . readMailbox

-- | Email address.  The @Eq@ instances compares the local part
-- case sensitively, and the domain part as described at 'Domain'.
--
-- Address "detail" (section of local part after a @'+'@ character;
-- also called "extension" or "subaddress") is part of the local
-- part.  Therefore addresses that differ in this aspect, for
-- example @alice+bank\@example.com@ and @alice+spam\@example.com@,
-- are unequal.
--
data AddrSpec =
    AddrSpec B.ByteString {- local part -}
             Domain
    deriving (Show, Eq, Generic, NFData)

data Address
    = Single Mailbox
    | Group T.Text {- display name -}
            [Mailbox]
    deriving (Show, Eq, Generic, NFData)

-- | A DNS name or "domain literal" (address literal).
-- DNS names are compared case-insensitively.
data Domain
    = DomainDotAtom (NonEmpty (CI B.ByteString) {- printable ascii -})
    | DomainLiteral B.ByteString
    deriving (Show, Eq, Generic, NFData)
