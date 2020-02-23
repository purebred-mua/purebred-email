{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , rfc5422DateTimeFormat
  , rfc5422DateTimeFormatLax

  -- * Serialisation
  , buildMessage
  , renderMessage
  , RenderMessage(..)
  , renderRFC5422Date
  , buildFields
  , buildField
  , renderMailbox
  , renderMailboxes
  , renderAddress
  , renderAddresses
  ) where

import Control.Applicative
import Data.Foldable (fold)
import Data.List (findIndex, intersperse)
import Data.List.NonEmpty (intersperse)
import Data.Semigroup ((<>))
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
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

import Data.RFC5322.Internal
import Data.RFC5322.Address.Types
import Data.MIME.Charset (decodeLenient)

type Header = (CI B.ByteString, B.ByteString)
newtype Headers = Headers [Header]
  deriving (Eq, Show, Generic, NFData)

instance Semigroup Headers where
  Headers a <> Headers b = Headers (a <> b)

instance Monoid Headers where
  mempty = Headers []

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
rfc5422DateTimeFormat :: String
rfc5422DateTimeFormat = "%a, %d %b %Y %T %z"

rfc5422DateTimeFormatLax :: String
rfc5422DateTimeFormatLax = "%a, %-d %b %Y %-H:%-M:%-S %z"

renderRFC5422Date :: UTCTime -> B.ByteString
renderRFC5422Date = Char8.pack . formatTime defaultTimeLocale rfc5422DateTimeFormat

-- §3.4 Address Specification
buildMailbox :: Mailbox -> Builder.Builder
buildMailbox (Mailbox n a) =
  maybe a' (\n' -> renderDisplayName n' <> "<" <> a' <> ">") n
  where
    a' = renderAddressSpec a

renderDisplayName :: T.Text -> Builder.Builder
renderDisplayName x =
    mconcat
        [ "\""
        , Builder.byteString (T.encodeUtf8 x)
        , "\" "]

renderMailboxes :: [Mailbox] -> B.ByteString
renderMailboxes = L.toStrict . Builder.toLazyByteString . buildMailboxes

buildMailboxes :: [Mailbox] -> Builder.Builder
buildMailboxes = fold . Data.List.intersperse ", " . fmap buildMailbox

renderMailbox :: Mailbox -> B.ByteString
renderMailbox = L.toStrict . Builder.toLazyByteString . buildMailbox

mailbox :: Parser Mailbox
mailbox = Mailbox <$> optional displayName <*> angleAddr
          <|> Mailbox Nothing <$> addressSpec

displayName :: Parser T.Text
displayName = decodeLenient <$> phrase

angleAddr :: Parser AddrSpec
angleAddr = optionalCFWS *>
  char8 '<' *> addressSpec <* char8 '>'
  <* optionalCFWS

renderAddressSpec :: AddrSpec -> Builder.Builder
renderAddressSpec (AddrSpec lp (DomainDotAtom b))
  | " " `B.isInfixOf` lp = "\"" <> buildLP <> "\"" <> rest
  | otherwise = buildLP <> rest
  where
    buildLP = Builder.byteString lp
    rest = "@" <> foldMap Builder.byteString (Data.List.NonEmpty.intersperse "." b)
renderAddressSpec (AddrSpec lp (DomainLiteral b)) =
  foldMap Builder.byteString [lp, "@", b]

addressSpec :: Parser AddrSpec
addressSpec = AddrSpec <$> localPart <*> (char8 '@' *> domain)

-- | Printable US-ASCII excl "[", "]", or "\"
isDtext :: Word8 -> Bool
isDtext c = (c >= 33 && c <= 90) || (c >= 94 && c <= 126)

domain :: Parser Domain
domain = (DomainDotAtom <$> dotAtom)
         <|> (DomainLiteral <$> domainLiteral)

mailboxList :: Parser [Mailbox]
mailboxList = mailbox `sepBy` char8 ','

renderAddresses :: [Address] -> B.ByteString
renderAddresses xs = B.intercalate ", " $ renderAddress <$> xs

renderAddress :: Address -> B.ByteString
renderAddress (Single m) = renderMailbox m
renderAddress (Group name xs) = T.encodeUtf8 name <> ":" <> renderMailboxes xs <> ";"

addressList :: Parser [Address]
addressList = address `sepBy` char8 ','

group :: Parser Address
group = Group <$> displayName <* char8 ':' <*> mailboxList <* char8 ';' <* optionalCFWS

address :: Parser Address
address = group <|> Single <$> mailbox

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

type family MessageContext a = s


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
    <> ": "
    <> Builder.byteString (foldUnstructured (B.length key) v)
    <> "\r\n"


-- | Render a field body with proper folding
--
-- Algorithm:
-- * Break the string on white space
-- * Use a counter which indicates a new folding line if it exceeds 77 characters
-- * Whenever we create a new line, concatenate all words back with white space and push it into the result
-- * The result is a list of byte strings, which is concatenated with \r\n\s
--
-- Notes:
--  * First take at this, so possibly very inefficient
--  * No other delimiters (e.g. commas, full stops, etc) are considered for
--    folding other than whitespace
--  * Attaches an additional whitespace when joining
--
foldUnstructured :: Int -> B.ByteString -> B.ByteString
foldUnstructured i b =
    let xs = chunk (i + 2) (Char8.words b) [] []
    in B.intercalate "\r\n " xs

chunk :: Int -> [B.ByteString] -> [B.ByteString] -> [B.ByteString] -> [B.ByteString]
chunk _ [] xs result = result <> [Char8.unwords xs]
chunk max' (x:rest) xs result = if (max' + B.length x + 1) >= 77
                                then result <> [Char8.unwords xs] <> chunk (B.length x + 1) rest [x] []
                                else result <> chunk (max' + B.length x + 1) rest (xs <> [x]) result

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
