-- This file is part of purebred-email
-- Copyright (C) 2018-2021  Róman Joost and Fraser Tweedale
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

{-# LANGUAGE OverloadedStrings #-}
{- |

@Text@ parsers and printers for mailbox and address types.

-}
module Data.IMF.Text
  (
    mailbox
  , mailboxList
  , readMailbox
  , address
  , addressList
  -- * Pretty printing
  , renderMailbox
  , renderMailboxes
  , renderAddress
  , renderAddresses
  , renderAddressSpec
  ) where

import Control.Applicative ((<|>), optional)
import Data.CaseInsensitive
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Char (isLetter)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Internal.Builder as Builder
import qualified Data.ByteString as B
import Data.Attoparsec.Text as A hiding (char, parse, take)
import Data.List.NonEmpty (intersperse)

import Data.MIME.Charset (decodeLenient)
import Data.IMF (Mailbox(..), Address(..), AddrSpec(..), Domain(..))
import Data.IMF.Syntax hiding (word)


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
    a' = buildAddressSpec a

renderAddresses :: [Address] -> T.Text
renderAddresses xs = T.intercalate ", " $ renderAddress <$> xs

renderAddress :: Address -> T.Text
renderAddress (Single m) = renderMailbox m
renderAddress (Group name xs) = name <> ":" <> renderMailboxes xs <> ";"

buildAddressSpec :: AddrSpec -> Builder.Builder
buildAddressSpec (AddrSpec lp (DomainDotAtom b))
  | " " `B.isInfixOf` lp = "\"" <> buildLP <> "\"" <> rest
  | otherwise = buildLP <> rest
  where
    buildLP = Builder.fromText $ decodeLenient lp
    rest = "@" <> foldMap (Builder.fromText . decodeLenient . original)
                          (Data.List.NonEmpty.intersperse "." b)
buildAddressSpec (AddrSpec lp (DomainLiteral b)) =
  foldMap Builder.fromText [decodeLenient lp, "@", decodeLenient b]

renderAddressSpec :: AddrSpec -> T.Text
renderAddressSpec = LT.toStrict . Builder.toLazyText . buildAddressSpec


-- §3.4 Address Specification
mailbox :: Parser Mailbox
mailbox = Mailbox <$> optional displayName <*> angleAddr
          <|> Mailbox Nothing <$> addressSpec

-- | Parse a (whole) string, returning an error @String@ or a 'Mailbox'.
readMailbox :: String -> Either String Mailbox
readMailbox = parseOnly (mailbox <* endOfInput) . T.pack

word :: Parser T.Text
word = optionalFWS *> foldMany1 (singleton . toChar <$> A.satisfy classes)
  where
    classes c = isLetter c || isAtext c

-- | Version of 'phrase' that does not process encoded-word
-- (we are parsing Text so will assume that the input does not
-- contain encoded words.  TODO this is probably wrong :)
phrase :: Parser T.Text
phrase = foldMany1Sep (singleton ' ') word

displayName :: Parser T.Text
displayName = phrase <|> quotedString

mailboxList :: Parser [Mailbox]
mailboxList = mailbox `sepBy` char ','

addressList :: Parser [Address]
addressList = address `sepBy` char ','

group :: Parser Address
group = Group <$> displayName <* char ':' <*> mailboxList <* char ';' <* optionalCFWS

address :: Parser Address
address = group <|> Single <$> mailbox

angleAddr :: Parser AddrSpec
angleAddr = optionalCFWS *>
  char '<' *> addressSpec <* char '>'
  <* optionalCFWS

addressSpec :: Parser AddrSpec
addressSpec = AddrSpec <$> (T.encodeUtf8 <$> localPart) <*> (char '@' *> domain)

domain :: Parser Domain
domain = (DomainDotAtom . fmap (mk . T.encodeUtf8) <$> dotAtom)
         <|> (DomainLiteral . T.encodeUtf8 <$> domainLiteral)
