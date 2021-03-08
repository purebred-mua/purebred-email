module Data.RFC5322.Address.Text where

import {-# SOURCE #-} Data.RFC5322.Address.Types

readMailbox :: String -> Either String Mailbox
