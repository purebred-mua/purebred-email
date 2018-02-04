module Data.RFC5322.Internal
  (
  -- * Case-insensitive value parsers
    ci
  , CI

  -- * Semigroup and monoid folding combinators
  , (<<>>)
  , foldMany
  , foldMany1

  -- * General parsers and combinators
  , skipTill
  , takeTill'
  ) where

import Control.Applicative ((<|>), liftA2, many)
import Control.Monad (void)
import Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Internal.Types as AT
import qualified Data.ByteString as B
import Data.CaseInsensitive (CI, FoldCase, mk)
import Data.Foldable (fold)
import Data.List.NonEmpty (fromList)
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

-- | Skip until the given parser succeeds
skipTill :: Parser a -> Parser ()
skipTill = void . spanTill

-- | Current offset in the input
position :: AT.Parser i Int
position = AT.Parser $ \t pos more _lose suc -> suc t pos more (AT.fromPos pos)

-- | Number of elements between current position and first position
-- at which parser matches (fails if it never matches).  Also
-- consumes the input on which the parser succeeds.
--
-- @@
-- λ> parseOnly (string "foo" *> spanTill (string ".")) "foobar."
-- Right 3
-- λ> parseOnly (string "foo" *> spanTill (string ".")) "foobar"
-- Left "not enough input"
-- @@
--
spanTill :: Parser a -> Parser Int
spanTill p = liftA2 (flip (-)) position q
  where
  q = position <* p <|> anyWord8 *> q

-- | Run the parser from the specified offset
--
-- @@
-- λ> parseOnly (seek 3 takeByteString) "foobar"
-- Right "bar"
-- @@
--
seek :: Int -> Parser a -> Parser a
seek pos p = AT.Parser $ \t _ more lose win -> AT.runParser p t (AT.Pos pos) more lose win

-- | Take until the parser matches (fails if it never matches).
--
-- @@
-- λ> parseOnly (takeTill' (string "bar") <* endOfInput) "foobar"
-- Right "foo"
-- @@
--
takeTill' :: Parser a -> Parser B.ByteString
takeTill' p = do
  pos <- position
  off <- spanTill p
  seek pos (A.take off) <* p
