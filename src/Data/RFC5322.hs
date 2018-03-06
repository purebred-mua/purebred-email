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

  -- * Parsers
  , parse
  , parsed
  , crlf
  , quotedString
  ) where

import Control.Applicative
import Control.Monad (void)
import Data.Word (Word8)

import Control.Lens
import Control.Lens.Cons.Extras (recons)
import Data.Attoparsec.ByteString as A hiding (parse)
import Data.Attoparsec.ByteString.Char8 (char8)
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString as B

import Data.RFC5322.Internal

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
  deriving (Show)

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
isAtext = inClass "A-Za-z0-9!#$%&'*+-/=?^_`{|}~"

isWsp :: Word8 -> Bool
isWsp = inClass "\t "

special :: Parser Word8
special = satisfy isSpecial

atext :: Parser Word8
atext = satisfy isAtext

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

-- | SP or TAB
wsp :: Parser Word8
wsp = satisfy isWsp


-- §3.2.2.  Folding White Space and Comments

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


-- §3.2.4.  Quoted Strings

quotedString :: Parser B.ByteString
quotedString =
  optionalCFWS *> dquote
  *> foldMany (optionalFWS <<>> qcontent) <<>> optionalFWS
  <* dquote <* optionalCFWS
  where
    qtext c = c == 33 || (c >= 35 && c <= 91) || (c >= 93 && c <= 126)
    qcontent = B.singleton <$> satisfy qtext -- FIXME <|> quoted-pair

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
