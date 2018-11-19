{-# LANGUAGE OverloadedStrings #-}
{- |

Parser for roundtripping Text based `Mailbox`es and addresses.

-}
module Data.RFC5322.Address.Text
  (
    mailbox
  , address
  -- * Pretty printing
  , renderMailbox
  , renderMailboxes
  ) where

import Control.Monad (void)
import Control.Applicative ((<|>), optional)
import Data.Foldable (fold)
import Data.Semigroup ((<>))
import Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Internal.Builder as Builder
import qualified Data.ByteString as B
import Data.Attoparsec.Text as A hiding (parse, take)
import Data.List.NonEmpty (intersperse)

import Data.MIME.Charset (decodeLenient)
import Data.RFC5322.Address.Types
import Data.RFC5322.Internal


renderMailboxes :: [Mailbox] -> T.Text
renderMailboxes = LT.toStrict . Builder.toLazyText . buildMailboxes

buildMailboxes :: [Mailbox] -> Builder.Builder
buildMailboxes = fold . Data.List.intersperse ", " . fmap buildMailbox

renderMailbox :: Mailbox -> T.Text
renderMailbox = LT.toStrict . Builder.toLazyText . buildMailbox

-- | Printing function to "pretty print" the mailbox for display purposes
buildMailbox :: Mailbox -> Builder.Builder
buildMailbox (Mailbox n a) =
  maybe a' (\n' -> "\"" <> Builder.fromText n' <> "\" " <> "<" <> a' <> ">") n
  where
    a' = renderAddressSpec a

renderAddressSpec :: AddrSpec -> Builder.Builder
renderAddressSpec (AddrSpec lp (DomainDotAtom b))
  | " " `B.isInfixOf` lp = "\"" <> buildLP <> "\"" <> rest
  | otherwise = buildLP <> rest
  where
    buildLP = Builder.fromText $ decodeLenient lp
    rest = "@" <> foldMap Builder.fromText (decodeLenient <$> Data.List.NonEmpty.intersperse "." b)
renderAddressSpec (AddrSpec lp (DomainLiteral b)) =
  foldMap Builder.fromText [decodeLenient lp, "@", decodeLenient b]


-- §3.4 Address Specification
mailbox :: Parser Mailbox
mailbox = Mailbox <$> optional displayName <*> angleAddr
          <|> Mailbox Nothing <$> addressSpec

displayName :: Parser T.Text
displayName = phrase

crlf :: Parser ()
crlf = void (string "\r\n" <|> string "\n")

isWsp :: Char -> Bool
isWsp = inClass "\t "

fws :: Parser T.Text
fws = optional (A.takeWhile isWsp *> crlf) *> takeWhile1 isWsp *> pure " "

mailboxList :: Parser [Mailbox]
mailboxList = mailbox `sepBy` char ','

group :: Parser Address
group = Group <$> displayName <* char ':' <*> mailboxList <* char ';' <* optionalCFWS

address :: Parser Address
address = group <|> Single <$> mailbox

-- | FWS collapsed to a single SPACE character, or empty string
--
optionalFWS :: Parser T.Text
optionalFWS = fws <|> pure mempty

-- | Printable ASCII excl. '(', ')', '\'
isCtext :: Char -> Bool
isCtext = go . fromEnum
  where go c = (c >= 33 && c <= 39) || (c >= 42 && c <= 91) || (c >= 93 && c <= 126)

ccontent :: Parser T.Text
ccontent = (T.singleton <$> satisfy isCtext) <|> comment

comment :: Parser T.Text
comment =
  char '('
  *> foldMany (optionalFWS <<>> ccontent) <* optionalFWS
  <* char ')'

cfws :: Parser T.Text
cfws =
  foldMany1 (optionalFWS <<>> comment) *> optionalFWS *> pure " "
  <|> fws

-- | CFWS collapsed to a single SPACE character, or empty string
--
optionalCFWS :: Parser T.Text
optionalCFWS = cfws <|> pure mempty

atext :: Parser Char
atext = satisfy isAtext

isAtext :: Char -> Bool
isAtext = inClass "-A-Za-z0-9!#$%&'*+/=?^_`{|}~"

atom :: Parser T.Text
atom = optionalCFWS *> foldMany1 (T.singleton <$> atext) <* optionalCFWS

word :: Parser T.Text
word = atom <|> quotedString

phrase :: Parser T.Text
phrase = foldMany1 word

-- §3.2.4.  Quoted Strings

isQtext :: Char -> Bool
isQtext = go . fromEnum
  where go c = c == 33 || (c >= 35 && c <= 91) || (c >= 93 && c <= 126)

quotedString :: Parser T.Text
quotedString =
  optionalCFWS *> dquote
  *> foldMany (optionalFWS <<>> qcontent) <<>> optionalFWS
  <* dquote <* optionalCFWS
  where
    qcontent = T.singleton <$> satisfy isQtext <|> T.singleton <$> quotedPair

isVchar :: Char -> Bool
isVchar = go . fromEnum
  where go c = c >= 0x21 && c <= 0x7e

vchar :: Parser Char
vchar = satisfy isVchar

wsp :: Parser Char
wsp = satisfy isWsp

quotedPair :: Parser Char
quotedPair = char '\\' *> (vchar <|> wsp)

dquote :: Parser Char
dquote = char '"'

angleAddr :: Parser AddrSpec
angleAddr = optionalCFWS *>
  char '<' *> addressSpec <* char '>'
  <* optionalCFWS

addressSpec :: Parser AddrSpec
addressSpec = AddrSpec <$> (T.encodeUtf8 <$> localPart) <*> (char '@' *> domain)

localPart :: Parser T.Text
localPart = dotAtom <|> quotedString

dotAtomText :: Parser T.Text
dotAtomText = takeWhile1 isAtext <<>> foldMany (A.char '.' *> (T.cons '.' <$> takeWhile1 isAtext))

dotAtom :: Parser T.Text
dotAtom = optionalCFWS *> dotAtomText <* optionalCFWS

-- | Printable US-ASCII excl "[", "]", or "\"
isDtext :: Char -> Bool
isDtext = go . fromEnum
  where go c = (c >= 33 && c <= 90) || (c >= 94 && c <= 126)

dText :: Parser Char
dText = satisfy isDtext

domainLiteral :: Parser T.Text
domainLiteral =
  optionalCFWS *> char '['
  *> foldMany (optionalFWS <<>> (T.singleton <$> dText) <<>> optionalFWS)
  <* char ']' <* optionalFWS

domain :: Parser Domain
domain = (DomainDotAtom <$> (pure . T.encodeUtf8 <$> dotAtom))
         <|> (DomainLiteral <$> (T.encodeUtf8 <$> domainLiteral))
