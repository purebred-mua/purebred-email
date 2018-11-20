{-# LANGUAGE ScopedTypeVariables #-}

module Data.RFC5322.Internal
  (
  -- * Case-insensitive value parsers
    ci
  , CI
  , original

  -- * Semigroup and monoid folding combinators
  , (<<>>)
  , foldMany
  , foldMany1

  -- * General parsers and combinators
  , skipTill
  , takeTill'

  -- * Efficient string search
  , skipTillString
  , takeTillString

  ) where

import Control.Applicative ((<|>), Alternative, liftA2, many)
import Control.Monad (void)
import Data.Attoparsec.ByteString as A
import Data.Attoparsec.Internal as A
import qualified Data.Attoparsec.Internal.Types as AT
import qualified Data.ByteString as B
import Data.ByteString.Search (indices)
import Data.CaseInsensitive (CI, FoldCase, mk, original)
import Data.Foldable (fold)
import Data.Functor (($>))
import Data.List.NonEmpty (fromList)
import Data.Semigroup (Semigroup((<>)))
import Data.Semigroup.Foldable (fold1)


-- | Modify a parser to produce a case-insensitive value
--
ci :: FoldCase s => Parser s -> Parser (CI s)
ci = fmap mk


-- | Combine two semigroup parsers into one
(<<>>) :: (Semigroup m, Applicative f) => f m -> f m -> f m
(<<>>) = liftA2 (<>)

-- | Parse zero or more values and fold them
foldMany :: (Monoid m, Alternative f) => f m -> f m
foldMany = fmap fold . many

-- | Parse one or more values and fold them
foldMany1 :: (Semigroup m, Alternative f) => f m -> f m
foldMany1 = fmap (fold1 . fromList) . A.many1

-- | Skip until the given parser succeeds
--
-- @@
-- λ> parseOnly (string "foo" *> skipTill (string ".") *> endOfInput) "foobar."
-- Right ()
-- @@
--
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

-- | Run the parser from the specified offset.
--
-- Should only be used to seek backwards, otherwise
-- you could seek beyond the buffer.  User beware.
--
-- @@
-- λ> parseOnly (seek 3 *> takeByteString) "foobar"
-- Right "bar"
-- @@
--
seek :: Int -> Parser ()
seek pos = AT.Parser $ \t _pos more _lose win -> win t (AT.Pos pos) more ()

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
  newPos <- position
  seek pos *> A.take off <* seek newPos

-- | Number of elements between current position and first position
-- at which the pattern matches (fails if it never matches).  Also
-- consumes the pattern.
--
-- Uses Boyer-Moore algorithm to efficiently search the input.
--
-- @@
-- λ> parseOnly (string "foo" *> spanTillString ".") "foobar."
-- Right 3
-- λ> parseOnly (string "foo" *> spanTillString ".") "foobar"
-- Left "not enough input"
-- @@
--
spanTillString :: B.ByteString -> Parser Int
spanTillString pat
  | B.null pat = position
  | otherwise = go
  where
  search = indices pat
  go = do
    start <- position
    buf <- takeBuffer
    case search buf of
      (offset:_) ->
        -- Pattern was found.  Seek to end of pattern and return the span
        seek (start + offset + B.length pat) $> offset
      _ ->
        -- We hit the end of the buffer without a match.  Seek to
        -- (length buf - length pat + 1), demand more input and go again.
        seek (B.length buf - B.length pat + 1) *> A.demandInput *> go

-- | Efficient skip, using Boyer-Moore to locate the pattern.
--
-- @@
-- λ> parseOnly (string "foo" *> skipTillString "." *> endOfInput) "foobar."
-- Right ()
-- @@
--
skipTillString :: B.ByteString -> Parser ()
skipTillString = void . spanTillString

-- | Efficient take, using Boyer-Moore to locate the pattern.
--
-- @@
-- λ> parseOnly (takeTillString "bar" <* endOfInput) "foobar"
-- Right "foo"
-- @@
--
takeTillString :: B.ByteString -> Parser B.ByteString
takeTillString pat = do
  pos <- position
  off <- spanTillString pat
  newPos <- position
  seek pos *> A.take off <* seek newPos

-- | /O(1)/ Take the rest of the buffer, but do not demand
-- any more input.
--
takeBuffer :: Parser B.ByteString
takeBuffer = do
  start <- position
  end <- bufSize
  A.take (end - start)

bufSize :: forall t. AT.Chunk t => AT.Parser t Int
bufSize = AT.Parser $
  \t pos more _lose win ->
    win t pos more
      (AT.fromPos $ AT.atBufferEnd (undefined :: t) t)
