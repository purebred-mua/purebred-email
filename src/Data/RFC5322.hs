{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

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
'message' :: ('Headers' -> Parser a) -> Parser (Message ctx a)
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
  , headers
  , body
  , Headers
  , header
  , Address(..)
  , address
  , AddrSpec(..)
  , Domain(..)
  , Mailbox(..)
  , mailbox

  -- * Parsers
  , parse
  , parsed
  , crlf
  , quotedString
  , field

  -- * Rendering
  , renderFields
  , renderField
  ) where

import Control.Applicative
import Control.Monad (void)
import Data.Word (Word8)
import Data.Semigroup ((<>))

import Data.List.NonEmpty (NonEmpty)
import Control.Lens
import Control.Lens.Cons.Extras (recons)
import Data.Attoparsec.ByteString as A hiding (parse)
import Data.Attoparsec.ByteString.Char8 (char8)
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as T

import Data.RFC5322.Internal
import Data.MIME.Charset (decodeLenient)

type Headers = [(CI B.ByteString, B.ByteString)]

-- | Get all values of the given header
header :: CI B.ByteString -> Fold Headers B.ByteString
header k = folded . filtered ((k ==) . fst) . _2

-- | Message type, parameterised over context and body type.  The
-- context type is not used in this module but is provided for uses
-- such as tracking the transfer/charset encoding state in MIME
-- messages.
--
data Message s a = Message Headers a
  deriving (Show, Eq)

headers :: Lens' (Message s a) Headers
headers f (Message h b) = fmap (\h' -> Message h' b) (f h)
{-# ANN headers ("HLint: ignore Avoid lambda" :: String) #-}

body :: Lens (Message ctx a) (Message ctx' b) a b
body f (Message h b) = fmap (\b' -> Message h b') (f b)
{-# ANN body ("HLint: ignore Avoid lambda" :: String) #-}


-- | Either CRLF or LF (lots of mail programs transform CRLF to LF)
crlf :: Parser ()
crlf = void (string "\r\n" <|> string "\n")

isSpecial :: Word8 -> Bool
isSpecial = inClass "()<>[]:;@\\,.\""

isAtext :: Word8 -> Bool
isAtext = inClass "-A-Za-z0-9!#$%&'*+/=?^_`{|}~"

isWsp :: Word8 -> Bool
isWsp = inClass "\t "

special :: Parser Word8
special = satisfy isSpecial

atext :: Parser Word8
atext = satisfy isAtext


-- §3.4 Address Specification
data Mailbox =
    Mailbox (Maybe T.Text {- display name -})
             AddrSpec
    deriving (Show,Eq)

mailbox :: Parser Mailbox
mailbox = Mailbox <$> optional displayName <*> angleAddr
          <|> Mailbox Nothing <$> addressSpec

displayName :: Parser T.Text
displayName = decodeLenient <$> phrase

word :: Parser B.ByteString
word = atom <|> quotedString

phrase :: Parser B.ByteString
phrase = foldMany1 word

angleAddr :: Parser AddrSpec
angleAddr = optionalCFWS *>
  char8 '<' *> addressSpec <* char8 '>'
  <* optionalCFWS

atom :: Parser B.ByteString
atom = optionalCFWS *> foldMany1 (B.singleton <$> atext) <* optionalCFWS

dotAtomText :: Parser B.ByteString
dotAtomText = takeWhile1 isAtext <<>> foldMany (char8 '.' *> (Char8.cons '.' <$> takeWhile1 isAtext))

dotAtom :: Parser B.ByteString
dotAtom = optionalCFWS *> dotAtomText <* optionalCFWS

data AddrSpec =
    AddrSpec B.ByteString {- local part -}
             Domain
    deriving (Show,Eq)

data Domain
    = DomainDotAtom (NonEmpty B.ByteString {- printable ascii -})
    | DomainLiteral B.ByteString
    deriving (Show,Eq)

addressSpec :: Parser AddrSpec
addressSpec = AddrSpec <$> localPart <*> (char8 '@' *> domain)

localPart :: Parser B.ByteString
localPart = dotAtom <|> quotedString

-- | Printable US-ASCII excl "[", "]", or "\"
isDtext :: Word8 -> Bool
isDtext c = (c >= 33 && c <= 90) || (c >= 94 && c <= 126)

dText :: Parser Word8
dText = satisfy isDtext

domainLiteral :: Parser B.ByteString
domainLiteral =
  optionalCFWS *> char8 '['
  *> foldMany (optionalFWS <<>> (B.singleton <$> dText) <<>> optionalFWS)
  <* char8 ']' <* optionalFWS

domain :: Parser Domain
domain = (DomainDotAtom <$> (pure <$> dotAtom))
         <|> (DomainLiteral <$> domainLiteral)

data Address
    = Single Mailbox
    | Group T.Text {- display name -}
            [Mailbox]
    deriving (Show,Eq)

mailboxList :: Parser [Mailbox]
mailboxList = mailbox `sepBy` char8 ','

group :: Parser Address
group = Group <$> displayName <* char8 ':' <*> mailboxList <* char8 ';' <* optionalCFWS

address :: Parser Address
address = group <|> Single <$> mailbox

-- §3.5.  Overall Message Syntax


-- | Parse a message, given function from headers to body parser
--
-- This parser does not handle the legitimate but obscure case
-- of a message with no body (empty body is fine, though).
--
message :: (Headers -> Parser a) -> Parser (Message s a)
message f = fields >>= \hdrs -> Message hdrs <$> (crlf *> f hdrs)

fields :: Parser Headers
fields = many field

-- | Render all headers
renderFields :: Headers -> B.ByteString
renderFields = foldl (\acc h -> acc <> renderField h) B.empty

renderField :: (CI B.ByteString, B.ByteString) -> B.ByteString
renderField (k,v) =
    let key = original k
        kLength = B.length key
    in B.concat [key, ": ", foldUnstructured kLength v, "\n"]

-- | SP or TAB
wsp :: Parser Word8
wsp = satisfy isWsp


-- §3.2.2.  Folding White Space and Comments
--
-- "The general rule is that wherever this specification allows for folding
-- white space (not simply WSP characters), a CRLF may be inserted before any
-- WSP."


fws :: Parser B.ByteString
fws = optional (A.takeWhile isWsp *> crlf) *> takeWhile1 isWsp *> pure " "

-- | FWS collapsed to a single SPACE character, or empty string
--
optionalFWS :: Parser B.ByteString
optionalFWS = fws <|> pure mempty

-- | Printable ASCII excl. '(', ')', '\'
isCtext :: Word8 -> Bool
isCtext c = (c >= 33 && c <= 39) || (c >= 42 && c <= 91) || (c >= 93 && c <= 126)

ccontent :: Parser B.ByteString
ccontent = (B.singleton <$> satisfy isCtext) <|> comment

comment :: Parser B.ByteString
comment =
  char8 '('
  *> foldMany (optionalFWS <<>> ccontent) <* optionalFWS
  <* char8 ')'

cfws :: Parser B.ByteString
cfws =
  foldMany1 (optionalFWS <<>> comment) *> optionalFWS *> pure " "
  <|> fws

-- | CFWS collapsed to a single SPACE character, or empty string
--
optionalCFWS :: Parser B.ByteString
optionalCFWS = cfws <|> pure mempty


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
--  * Assumes that we strip of a whitespace, so attaches an additional
--    whitespace when joining
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

-- §3.2.4.  Quoted Strings

quotedString :: Parser B.ByteString
quotedString =
  optionalCFWS *> dquote
  *> foldMany (optionalFWS <<>> qcontent) <<>> optionalFWS
  <* dquote <* optionalCFWS
  where
    qtext c = c == 33 || (c >= 35 && c <= 91) || (c >= 93 && c <= 126)
    qcontent = B.singleton <$> satisfy qtext <|> B.singleton <$> quotedPair

quotedPair :: Parser Word8
quotedPair = char8 '\\' *> (vchar <|> wsp)

dquote :: Parser Word8
dquote = char8 '"'

-- §3.6.8.  Optional fields

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

vchar :: Parser Word8
vchar = satisfy (\c -> c >= 33 && c <= 126)


-- | Given a parser, construct a 'Fold'
--
-- See 'parse' for discussion of performance.
--
parsed :: (Cons s s Word8 Word8) => Parser a -> Fold s a
parsed p = to (parse p) . folded
{-# INLINE parsed #-}

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
