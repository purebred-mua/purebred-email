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

data Mailbox =
    Mailbox (Maybe T.Text {- display name -})
             AddrSpec
    deriving (Show, Eq, Generic, NFData)

data AddrSpec =
    AddrSpec B.ByteString {- local part -}
             Domain
    deriving (Show, Eq, Generic, NFData)

data Address
    = Single Mailbox
    | Group T.Text {- display name -}
            [Mailbox]
    deriving (Show, Eq, Generic, NFData)

data Domain
    = DomainDotAtom (NonEmpty B.ByteString {- printable ascii -})
    | DomainLiteral B.ByteString
    deriving (Show, Eq, Generic, NFData)
