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

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.IMF.Address.Types
  (
    Mailbox(..)
  , Address(..)
  , AddrSpec(..)
  , Domain(..)
  ) where

import Control.DeepSeq (NFData)
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

import Data.CaseInsensitive

import {-# SOURCE #-} Data.IMF.Address.Text (readMailbox)

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
