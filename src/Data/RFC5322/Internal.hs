module Data.RFC5322.Internal
  (
  -- * Case-insensitive value parsers
    ci
  , CI

  -- * Semigroup and monoid folding combinators
  , (<<>>)
  , foldMany
  , foldMany1
  ) where

import Control.Applicative (liftA2, many)
import Data.Attoparsec.ByteString
import Data.CaseInsensitive (CI, FoldCase, mk)
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.Semigroup (Semigroup((<>)))
import Data.Semigroup.Foldable (fold1)


-- | Modify a parser to produce a case-insensitive value
--
ci :: FoldCase s => Parser s -> Parser (CI s)
ci = fmap mk


-- | Combine two semigroup parsers into one
(<<>>) :: Semigroup m => Parser m -> Parser m -> Parser m
(<<>>) = liftA2 (<>)

-- | Parse zero or more values and fold them
foldMany :: (Monoid m) => Parser m -> Parser m
foldMany = fmap fold . many

-- | Parse one or more values and fold them
foldMany1 :: (Semigroup m) => Parser m -> Parser m
foldMany1 = fmap (fold1 . fromList) . many1
