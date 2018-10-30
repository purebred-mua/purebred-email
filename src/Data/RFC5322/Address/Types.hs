
module Data.RFC5322.Address.Types
  (
    Mailbox(..)
  , Address(..)
  , AddrSpec(..)
  , Domain(..)
  ) where

import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.List.NonEmpty (NonEmpty)

data Mailbox =
    Mailbox (Maybe T.Text {- display name -})
             AddrSpec
    deriving (Show,Eq)

data AddrSpec =
    AddrSpec B.ByteString {- local part -}
             Domain
    deriving (Show,Eq)

data Address
    = Single Mailbox
    | Group T.Text {- display name -}
            [Mailbox]
    deriving (Show,Eq)

data Domain
    = DomainDotAtom (NonEmpty B.ByteString {- printable ascii -})
    | DomainLiteral B.ByteString
    deriving (Show,Eq)
