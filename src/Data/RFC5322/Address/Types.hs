{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.RFC5322.Address.Types
  (
    Mailbox(..)
  , Address(..)
  , AddrSpec(..)
  , Domain(..)
  ) where

import Control.DeepSeq (NFData)
import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

import Data.CaseInsensitive

-- | Email address with optional display name.
-- The @Eq@ instance compares the display name case
-- sensitively and the address as described at 'AddrSpec'.
--
data Mailbox =
    Mailbox (Maybe T.Text {- display name -})
             AddrSpec
    deriving (Show, Eq, Generic, NFData)

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
