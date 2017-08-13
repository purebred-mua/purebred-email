module Data.RFC5322.Internal
  (
    ci
  , CI
  ) where

import Data.Attoparsec.ByteString
import Data.CaseInsensitive (CI, FoldCase, mk)


-- | Modify a parser to produce a case-insensitive value
--
ci :: FoldCase s => Parser s -> Parser (CI s)
ci = fmap mk
