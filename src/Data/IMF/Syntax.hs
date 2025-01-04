-- This file is part of purebred-email
-- Copyright (C) 2017-2021  Fraser Tweedale and Róman Joost
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

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |

Parsers for low-level productions in the Internet Message Format.
These parsers are used throughout this library and may be useful
to other programs.

-}
module Data.IMF.Syntax
  (
  -- * Case-insensitive value parsers
    ci
  , CI
  , mk
  , original

  -- * Abstract character parsers
  , wsp
  , fws
  , optionalFWS
  , optionalCFWS
  , crlf
  , vchar
  , word
  , dquote
  , quotedString
  , dotAtomText
  , dotAtom
  , localPart
  , domainLiteral
  , IsChar(..)
  , char
  , CharParsing(..)
  , SM

  -- ** Helpers for building parsers
  , isAtext
  , isQtext
  , isVchar
  , isWsp

  -- * Semigroup and monoid folding combinators
  , (<<>>)
  , foldMany
  , foldMany1
  , foldMany1Sep

  -- * General parsers and combinators
  , skipTill
  , takeTill'

  -- * Efficient string search
  , skipTillString
  , takeTillString

  ) where

import Prelude hiding (takeWhile)
import Control.Applicative ((<|>), Alternative, liftA2, many, optional, some)
import Control.Monad (void)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Internal as A
import qualified Data.Attoparsec.Internal.Types as AT
import qualified Data.Attoparsec.Text as AText
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w, w2c)
import Data.ByteString.Search (indices)
import Data.CaseInsensitive (CI, FoldCase, mk, original)
import Data.Char (chr)
import Data.Foldable (fold)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty, fromList, intersperse)
import Data.Semigroup.Foldable (fold1)
import qualified Data.Text as T
import Data.Word (Word8)


-- | Constraint synonym to handle the Semigroup Monoid Proposal
-- transition gracefully.
type SM a = Monoid a

class IsChar a where
  toChar :: a -> Char
  fromChar :: Char -> a

instance IsChar Char where
  toChar = id
  fromChar = id

instance IsChar Word8 where
  toChar = w2c
  fromChar = c2w

class IsChar a => CharParsing f s a | s -> a, a -> f s where
  singleton :: Char -> s
  satisfy :: (Char -> Bool) -> (f s) a
  takeWhile :: (Char -> Bool) -> (f s) s
  takeWhile1 :: (Char -> Bool) -> (f s) s

instance CharParsing AT.Parser B.ByteString Word8 where
  singleton = B.singleton . c2w
  satisfy f = A.satisfy (f . w2c)
  takeWhile f = A.takeWhile (f . w2c)
  takeWhile1 f = A.takeWhile1 (f . w2c)

instance CharParsing AT.Parser T.Text Char where
  singleton = T.singleton
  satisfy = AText.satisfy
  takeWhile = AText.takeWhile
  takeWhile1 = AText.takeWhile1

char :: CharParsing f s a => Char -> (f s) a
char c = satisfy (== c)

isWsp :: IsChar c => c -> Bool
isWsp = AText.inClass "\t " . toChar

wsp :: CharParsing f s a => (f s) a
wsp = satisfy isWsp

isVchar :: IsChar c => c -> Bool
isVchar c =
  let c' = toChar c
  in c' >= chr 0x21 && c' <= chr 0x7e

vchar :: CharParsing f s a => (f s) a
vchar = satisfy isVchar

dquote :: CharParsing f s a => (f s) a
dquote = char '"'

quotedPair :: (Alternative (f s)) => CharParsing f s a => (f s) a
quotedPair = char '\\' *> (vchar <|> wsp)

-- §3.2.4.  Quoted Strings

isQtext :: IsChar c => c -> Bool
isQtext c' =
  let c = toChar c'
  in
    c == chr 33
    || (c >= chr 35 && c <= chr 91)
    || (c >= chr 93 && c <= chr 126)

quotedString :: (Alternative (f s), CharParsing f s a, SM s) => (f s) s
quotedString =
  optionalCFWS *> dquote
  *> foldMany (optionalFWS <<>> qcontent) <<>> optionalFWS
  <* dquote <* optionalCFWS
  where
    qcontent =
      (singleton . toChar <$> satisfy isQtext)
      <|> (singleton . toChar <$> quotedPair)

isAtext :: IsChar c => c -> Bool
isAtext = AText.inClass "-A-Za-z0-9!#$%&'*+/=?^_`{|}~" . toChar

atext :: CharParsing f s a => (f s) a
atext = satisfy isAtext

-- | Either CRLF or LF (lots of mail programs transform CRLF to LF)
crlf :: (Alternative (f s)) => CharParsing f s a => (f s) ()
crlf = void ((char '\r' *> char '\n') <|> char '\n')

-- §3.2.2.  Folding White Space and Comments
--
-- "The general rule is that wherever this specification allows for folding
-- white space (not simply WSP characters), a CRLF may be inserted before any
-- WSP."

-- | Folding white space (FWS).  A run of one or more whitespace
-- characters.  Returns a single SPACE character.
fws :: (Alternative (f s), CharParsing f s a) => (f s) s
fws =
  -- obs-FWS is more permissive, so must come first.  This means
  -- that the second branch is unused, but keep it anyway for
  -- completeness.
  obsFWS
  <|>
  --    FWS             =   ([*WSP CRLF] 1*WSP) /  obs-FWS
  optional (takeWhile isWsp *> crlf) *> takeWhile1 isWsp $> singleton ' '

-- | Obsolete Folding White Space:
-- https://www.rfc-editor.org/errata/eid1908
--
-- @
-- obs-FWS         =   1*([CRLF] WSP)
-- @
--
obsFWS :: (Alternative (f s), CharParsing f s a) => (f s) s
obsFWS = some (optional crlf *> wsp) $> singleton ' '

-- | FWS collapsed to a single SPACE character, or empty string
--
optionalFWS :: (Alternative (f s), CharParsing f s a, Monoid s) => (f s) s
optionalFWS = fws <|> pure mempty

-- | Printable ASCII excl. '(', ')', '\'
isCtext :: Char -> Bool
isCtext c =
  c >= chr 33 && c <= chr 39
  || c >= chr 42 && c <= chr 91
  || c >= chr 93 && c <= chr 126

ccontent :: (Alternative (f s), CharParsing f s a, SM s) => (f s) s
ccontent = (singleton . toChar <$> satisfy isCtext) <|> comment

comment :: (Alternative (f s), CharParsing f s a, SM s) => (f s) s
comment =
  char '(' *> foldMany (optionalFWS <<>> ccontent) <* optionalFWS <* char ')'

cfws :: (Alternative (f s), CharParsing f s a, SM s) => (f s) s
cfws =
  ((foldMany1 (optionalFWS <<>> comment) *> optionalFWS) $> singleton ' ')
  <|> fws

-- | CFWS collapsed to a single SPACE character, or empty string
--
optionalCFWS :: (Alternative (f s), CharParsing f s a, SM s) => (f s) s
optionalCFWS = cfws <|> pure mempty

atom :: (Alternative (f s), CharParsing f s a, SM s) => (f s) s
atom = optionalCFWS *> foldMany1 (singleton . toChar <$> atext) <* optionalCFWS

word :: (Alternative (f s), CharParsing f s a, SM s) => (f s) s
word = atom <|> quotedString

dotAtomText :: (Alternative (f s), CharParsing f s a) => (f s) (NonEmpty s)
dotAtomText = fromList <$> (takeWhile1 isAtext `A.sepBy1` char '.')

dotAtom :: (Alternative (f s), CharParsing f s a, SM s) => (f s) (NonEmpty s)
dotAtom = optionalCFWS *> dotAtomText <* optionalCFWS

localPart :: (Alternative (f s), CharParsing f s a, SM s) => (f s) s
localPart = (fold . intersperse (singleton '.') <$> dotAtom) <|> quotedString

-- | Printable US-ASCII excl "[", "]", or "\"
isDtext :: Char -> Bool
isDtext c = (c >= chr 33 && c <= chr 90) || (c >= chr 94 && c <= chr 126)

dText :: CharParsing f s a => (f s) a
dText = satisfy isDtext

domainLiteral :: (Alternative (f s), CharParsing f s a, SM s) => (f s) s
domainLiteral =
  optionalCFWS *> char '['
  *> foldMany (optionalFWS <<>> (singleton . toChar <$> dText) <<>> optionalFWS)
  <* char ']' <* optionalFWS


-- | Modify a parser to produce a case-insensitive value
--
ci :: FoldCase s => A.Parser s -> A.Parser (CI s)
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

-- | Parse one or more values and fold them with a separating element
foldMany1Sep :: (Semigroup m, Alternative f) => m -> f m -> f m
foldMany1Sep sep = fmap (fold1 . intersperse sep . fromList) . A.many1

-- | Skip until the given parser succeeds
--
-- @@
-- λ> parseOnly (string "foo" *> skipTill (string ".") *> endOfInput) "foobar."
-- Right ()
-- @@
--
skipTill :: A.Parser a -> A.Parser ()
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
spanTill :: A.Parser a -> A.Parser Int
spanTill p = liftA2 (flip (-)) position q
  where
  q = position <* p <|> A.anyWord8 *> q

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
seek :: Int -> A.Parser ()
seek pos = AT.Parser $ \t _pos more _lose win -> win t (AT.Pos pos) more ()

-- | Take until the parser matches (fails if it never matches).
--
-- @@
-- λ> parseOnly (takeTill' (string "bar") <* endOfInput) "foobar"
-- Right "foo"
-- @@
--
takeTill' :: A.Parser a -> A.Parser B.ByteString
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
spanTillString :: B.ByteString -> A.Parser Int
spanTillString pat
  | B.null pat = position
  | otherwise = position >>= go
  where
  search = indices pat
  go start = do
    pos <- position
    buf <- takeBuffer
    case search buf of
      (offset:_) ->
        -- Pattern was found.  Seek to end of pattern and return the span
        seek (pos + offset + B.length pat) $> pos + offset - start
      _ ->
        -- We hit the end of the buffer without a match.  Seek to
        -- (length buf - length pat), demand more input and go again.
        seek (max start (B.length buf - B.length pat)) *> A.demandInput *> go start

-- | Efficient skip, using Boyer-Moore to locate the pattern.
--
-- @@
-- λ> parseOnly (string "foo" *> skipTillString "." *> endOfInput) "foobar."
-- Right ()
-- @@
--
skipTillString :: B.ByteString -> A.Parser ()
skipTillString = void . spanTillString

-- | Efficient take, using Boyer-Moore to locate the pattern.
--
-- @@
-- λ> parseOnly (takeTillString "bar" <* endOfInput) "foobar"
-- Right "foo"
-- @@
--
takeTillString :: B.ByteString -> A.Parser B.ByteString
takeTillString pat = do
  pos <- position
  off <- spanTillString pat
  newPos <- position
  seek pos *> A.take off <* seek newPos

-- | /O(1)/ Take the rest of the buffer, but do not demand
-- any more input.
--
takeBuffer :: A.Parser B.ByteString
takeBuffer = do
  start <- position
  end <- bufSize
  A.take (end - start)

bufSize :: forall t. AT.Chunk t => AT.Parser t Int
bufSize = AT.Parser $
  \t pos more _lose win ->
    win t pos more
      (AT.fromPos $ AT.atBufferEnd (undefined :: t) t)
