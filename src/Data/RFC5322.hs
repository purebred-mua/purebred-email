{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Data.RFC5322
  where

{- |

Email messages.  Deals specifically with RFC 5322, which is stricter
than RFC 822 or RFC 2822.  If you have to deal with messages that
comply with the older specifications but not RFC 5322, preprocess
the input and massage it to be RFC 5322 compliant.

This parser allows LF line endings in addition to CRLF (RFC 5322
demands CRLF but LF-only is common in on-disk formats).

-}

import Control.Applicative
import Control.Monad (join, void)
import Data.Foldable (fold)
import Data.Semigroup ((<>))
import Data.Word (Word8)

import Control.Lens
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B

import Data.RFC5322.Internal

type Headers = [(CI B.ByteString, B.ByteString)]

header :: CI B.ByteString -> Fold Headers B.ByteString
header k = folded . filtered ((k ==) . fst) . _2

data RFC5322 a = RFC5322 Headers (Maybe a)
  deriving (Show)

type Body = B.ByteString


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

message :: Parser (RFC5322 Body)
message = RFC5322 <$> fields <*> optional (crlf *> body) <* endOfInput

body :: Parser B.ByteString
body = takeByteString

fields = many field

-- | SP or TAB
wsp :: Parser Word8
wsp = satisfy isWsp


-- §3.2.2.  Folding White Space and Comments

fws :: Parser [Word8]
fws = optional (many wsp *> crlf) *> many1 wsp *> pure [32 {-SPACE-}]

-- | FWS collapsed to a single SPACE character, or empty string
--
optionalFWS :: Parser [Word8]
optionalFWS = fws <|> pure []

-- | Printable ASCII excl. '(', ')', '\'
isCtext :: Word8 -> Bool
isCtext c = (c >= 33 && c <= 39) || (c >= 42 && c <= 91) || (c >= 93 && c <= 126)

ccontent :: Parser [Word8]
ccontent = ((:[]) <$> satisfy isCtext) <|> comment

comment :: Parser [Word8]
comment = word8 40 {-(-} *> (fold <$> many ((<>) <$> optionalFWS <*> ccontent)) <* optionalFWS <* word8 41 {-)-}

cfws :: Parser [Word8]
cfws = (join <$> many1 ((<>) <$> optionalFWS <*> comment)) <* optionalFWS <|> fws

-- | CFWS collapsed to a single SPACE character, or empty string
--
optionalCFWS :: Parser [Word8]
optionalCFWS = cfws <|> pure []


-- §3.6.8.  Optional fields

-- | Printable ASCII excl. ':'
isFtext :: Word8 -> Bool
isFtext c = (c >= 33 && c <= 57) || (c >= 59 && c <= 126)

field :: Parser (CI B.ByteString, B.ByteString)
field = (,)
  <$> ci (takeWhile1 isFtext)
  <*  word8 58 {-:-}
  <*> unstructured <* crlf

unstructured :: Parser B.ByteString
unstructured =
  foldMap B.pack <$> many ((<>) <$> optionalFWS <*> ((:[]) <$> vchar))
  <* many wsp -- FIXME: retain wsp (see https://tools.ietf.org/html/rfc5322#section-3.2.5)

vchar :: Parser Word8
vchar = satisfy (\c -> c >= 33 && c <= 126)
