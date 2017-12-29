{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{- |

Email messages.  Deals specifically with RFC 5322, which is stricter
than RFC 822 or RFC 2822.  If you have to deal with messages that
comply with the older specifications but not RFC 5322, preprocess
the input and massage it to be RFC 5322 compliant.

This parser allows LF line endings in addition to CRLF (RFC 5322
demands CRLF but LF-only is common in on-disk formats).

-}
module Data.RFC5322
  (
  -- * Message types
    RFC5322(..)
  , message
  , Headers
  , header

  -- * Parsers
  , parsed
  , crlf
  , quotedString
  ) where

import Control.Applicative
import Control.Monad (void)
import Data.Word (Word8)

import Control.Lens
import Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString.Char8 (char8)
import qualified Data.ByteString as B

import Data.RFC5322.Internal

type Headers = [(CI B.ByteString, B.ByteString)]

header :: CI B.ByteString -> Fold Headers B.ByteString
header k = folded . filtered ((k ==) . fst) . _2

-- | Message type, parameterised over body type
data RFC5322 a = RFC5322 Headers a
  deriving (Show)


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
message :: (Headers -> Parser a) -> Parser (RFC5322 a)
message f = fields >>= \hdrs -> RFC5322 hdrs <$> (crlf *> f hdrs)

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
parsed :: Parser a -> Fold B.ByteString a
parsed p = to (parseOnly p) . folded
