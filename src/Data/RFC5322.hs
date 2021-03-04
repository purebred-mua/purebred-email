{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |

Email messages.  Deals specifically with RFC 5322, which is stricter
than RFC 822 or RFC 2822.  If you have to deal with messages that
comply with the older specifications but not RFC 5322, preprocess
the input and massage it to be RFC 5322 compliant.

This parser allows LF line endings in addition to CRLF (RFC 5322
demands CRLF but LF-only is common in on-disk formats).

The main parsing function is 'message'.  It takes a second function
that can inspect the headers to determine how to parse the body.

@
'message' :: ('Headers' -> 'BodyHandler' a) -> Parser (Message ctx a)
@

The 'Message' type is parameterised over the body type, and a
phantom type that can be used for context.

@
data 'Message' ctx a = Message 'Headers' a
@

Headers and body can be accessed via the 'headers', 'header' and
'body' optics.

@
'headers' :: Lens' (Message ctx a) Headers
'header' :: CI B.ByteString -> Fold Headers B.ByteString
'body' :: Lens (Message ctx a) (Message ctx' b) a b
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
      B.putStrLn $ "subject: " <> foldOf ('headers' . 'header' "subject") msg
      putStrLn $ "num headers: " <> show (length (view 'headers' msg))
      putStrLn $ "body length: " <> show (B.length (view 'body' msg))
@

-}
module Data.RFC5322
  (
  -- * Message types
    Message(..)
  , message
  , MessageContext
  , BodyHandler(..)
  , body
  , EqMessage(..)

  -- ** Headers
  , Header
  , HasHeaders(..)
  , header
  , headerList
  , Headers(..)

  -- *** Date and Time
  , headerDate
  , dateTime

  -- *** Originator
  , headerFrom
  , headerReplyTo

  -- *** Destination Address
  , headerTo
  , headerCC
  , headerBCC

  -- *** Informational
  , headerSubject

  -- *** Arbitrary headers
  , headerText

  -- ** Addresses
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
import Data.Foldable (fold)
import Data.List (findIndex, intersperse)
import Data.List.NonEmpty (intersperse)
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

import Data.RFC5322.Internal
  ( CI, ci, original
  , (<<>>), foldMany, foldMany1Sep
  , fromChar, isAtext, isQtext, isVchar, isWsp
  , optionalCFWS, word, wsp, vchar, optionalFWS, crlf
  , domainLiteral, dotAtom, localPart, quotedString
  )
import Data.RFC5322.DateTime (dateTime)
import Data.RFC5322.Address.Types
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

isSpecial :: Word8 -> Bool
isSpecial = inClass "()<>[]:;@\\,.\""

special :: Parser Word8
special = satisfy isSpecial


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
  fmap (decodeEncodedWord charsets) ("=?" *> encodedWord)
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
    rest = "@" <> foldMap Builder.byteString (Data.List.NonEmpty.intersperse "." b)
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
domain = (DomainDotAtom <$> dotAtom)
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
  tweakHeaders :: Headers -> Headers
  tweakHeaders = id

-- | Construct a 'Builder.Builder' for the message.  This allows efficient
-- streaming to IO handles.
--
buildMessage :: forall ctx a. (RenderMessage a) => Message ctx a -> Builder.Builder
buildMessage (Message h b) =
  buildFields (tweakHeaders @a h)
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
    | col + 1 + B.length chunk < limit =
        -- there is room for the chunk
        " " <> Builder.byteString chunk <> go chunks (col + 1 + B.length chunk)
    | col <= 1 =
        -- there isn't room for the chunk, but we are at the
        -- beginning of the line so add it here anyway (otherwise
        -- we will add "\r\n" and recurse forever
        " " <> Builder.byteString chunk <> "\r\n" <> go chunks 0
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
-- The input is convered to a /lazy/ @ByteString@.
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
