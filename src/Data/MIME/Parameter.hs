-- This file is part of purebred-email
-- Copyright (C) 2018-2021  Fraser Tweedale
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

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{- |

MIME parameters, per RFC 2045 and RFC 2231.

RFC 2231 defines a mechanism for parameter continuations (for long
parameters), encoding of non-ASCII characters, and charset and
language annotation.  The most common use of these capabilities is
in the @Content-Disposition@ header, for the @filename@ parameter.

This module provides types and functions for working with parameters.

-}
module Data.MIME.Parameter
  (
    Parameters(..)
  , parameterList
  , parameter
  , rawParameter
  , newParameter

  , ParameterValue(..)
  , EncodedParameterValue
  , DecodedParameterValue
  , value

  , HasParameters(..)
  ) where

import Control.Applicative ((<|>), optional)
import Data.Foldable (fold)
import Data.Functor (($>))
import Data.Semigroup (Sum(..), Max(..))
import Data.String (IsString(..))
import Data.Word (Word8)
import Data.Void (Void)
import Foreign (withForeignPtr, plusPtr, minusPtr, peek, peekByteOff, poke)
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafeDupablePerformIO)

import Control.DeepSeq (NFData)
import Control.Lens
import Control.Lens.Cons.Extras (recons)
import Data.Attoparsec.ByteString.Char8 hiding (take)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Char8 as C
import Data.CaseInsensitive (CI, foldedCase, mk, original)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.MIME.Charset
import Data.MIME.Internal
import Data.IMF.Internal (ci, isQtext, isVchar)

type RawParameters = [(CI B.ByteString, B.ByteString)]
-- | Header parameters.  Used for some headers including Content-Type
-- and Content-Disposition.  This type handles parameter continuations
-- and optional charset and language information (RFC 2231).
--
newtype Parameters = Parameters [(CI B.ByteString, B.ByteString)]
  deriving (Eq, Show, Generic, NFData)

instance Semigroup Parameters where
  Parameters a <> Parameters b = Parameters (a <> b)

instance Monoid Parameters where
  mempty = Parameters []

type instance Index Parameters = CI B.ByteString
type instance IxValue Parameters = EncodedParameterValue

paramiso :: Iso' Parameters [(CI B.ByteString, B.ByteString)]
paramiso = iso (\(Parameters raw) -> raw) Parameters

-- Traverses 0 or 1 instances of a parameter, which may consist of
-- one or more raw parameters.
instance Ixed Parameters where
  ix k = paramiso . l
    where
    l f kv = case getParameter k kv of
      Nothing -> pure kv
      Just v -> (\v' -> setParam k v' kv) <$> f v

-- | Set the parameter (which may need to use the parameter
-- continuation mechanism).
setParam :: CI B.ByteString -> EncodedParameterValue -> RawParameters -> RawParameters
setParam k v = (renderParam k v <>) . deleteParam k

-- | Turn a ParameterValue into a list of raw parameters
--
-- FIXME: currently does not do continutations etc.
-- 'ParameterValue' value is used as-is.
renderParam :: CI B.ByteString -> EncodedParameterValue -> [(CI B.ByteString, B.ByteString)]
renderParam k pv = case pv of
  ParameterValue Nothing Nothing v -> case extEncode minBound v of
    (Plain, v') -> [(k, v')]
    (Quoted, v') -> [(k, "\"" <> v' <> "\"")]
    (Extended, v') -> [(k <> "*", "''" <> v')]
  ParameterValue cs lang v ->
    -- charset or lang has been specified; force extended syntax
    [(k <> "*", f cs <> "'" <> f lang <> "'" <> snd (extEncode Extended v))]
  where
  f = maybe "" original

-- | Delete all raw keys that are "part of" the extended/continued
-- parameter.
deleteParam :: CI B.ByteString -> RawParameters -> RawParameters
deleteParam k = filter (not . test . fst)
  where
  test x =
    x == k
    || (foldedCase k <> "*") `B.isPrefixOf` foldedCase x

instance At Parameters where
  at k = paramiso . l
    where
    l :: Lens' RawParameters (Maybe EncodedParameterValue)
    l f kv =
      let
        g Nothing = deleteParam k kv
        g (Just v) = (setParam k v . deleteParam k) kv
      in
        g <$> f (getParameter k kv)

data Continued = Continued | NotContinued
  deriving (Show)
data Encoded = Encoded | NotEncoded
  deriving (Show)

-- | Not percent-decoded.  'Encoded' indicates whether
-- percent-decoding is required.  'Continued' indicates whether
-- there are more sections to follow
--
data InitialSection = InitialSection Continued Encoded B.ByteString
  deriving (Show)

-- | Not percent-decoded.  'Encoded' indicates whether
-- percent-decoding is required.
--
data OtherSection = OtherSection Encoded B.ByteString
  deriving (Show)

initialSection
  :: CI B.ByteString
  -> RawParameters
  -> Maybe InitialSection
initialSection k m =
  InitialSection NotContinued NotEncoded <$> lookup k m
  <|> InitialSection Continued NotEncoded <$> lookup (k <> "*0") m
  <|> InitialSection NotContinued Encoded <$> lookup (k <> "*") m
  <|> InitialSection Continued Encoded <$> lookup (k <> "*0*") m

otherSection
  :: CI B.ByteString
  -> Int
  -> RawParameters
  -> Maybe OtherSection
otherSection k i m =
  OtherSection NotEncoded <$> lookup (k <> "*" <> i') m
  <|> OtherSection Encoded <$> lookup (k <> "*" <> i' <> "*") m
  where
    i' = mk $ C.pack (show i)

data ParameterValue cs a = ParameterValue
  (Maybe cs)                 -- charset
  (Maybe (CI B.ByteString))  -- language
  a                          -- value
  deriving (Eq, Show, Generic, NFData)

type EncodedParameterValue = ParameterValue CharsetName B.ByteString
type DecodedParameterValue = ParameterValue Void T.Text

-- | Parameter value with no language.
instance IsString DecodedParameterValue where
  fromString = ParameterValue Nothing Nothing . T.pack

-- | Parameter value with no language, encoded either in @us-ascii@
-- or @utf-8.
instance IsString EncodedParameterValue where
  fromString = charsetEncode . fromString

value :: Lens (ParameterValue cs a) (ParameterValue cs b) a b
value f (ParameterValue a b c) = ParameterValue a b <$> f c

charset :: Lens (ParameterValue cs a) (ParameterValue cs' a) (Maybe cs) (Maybe cs')
charset f (ParameterValue a b c) = (\a' -> ParameterValue a' b c) <$> f a


-- | Convenience function to construct a parameter value.
-- If you need to to specify language, use the 'ParameterValue'
-- constructor directly.
--
newParameter :: Cons s s Char Char => s -> EncodedParameterValue
newParameter = charsetEncode . ParameterValue Nothing Nothing . view recons


-- | The default charset @us-ascii@ is implied by the abstract of
-- RFC 2231 which states: /This memo defines … a means to specify
-- parameter values in character sets other than US-ASCII/.
--
-- When encoding, 'utf-8' is always used, but if the whole string
-- contains only ASCII characters then the charset declaration is
-- omitted (so that it can be encoded as a non-extended parameter).
--
instance HasCharset EncodedParameterValue where
  type Decoded EncodedParameterValue = DecodedParameterValue
  charsetName = to $ \(ParameterValue name _ _) -> name <|> Just "us-ascii"
  charsetData = value
  charsetDecoded m = to $ \a -> (\t -> (set charset Nothing . set value t) a) <$> view (charsetText m) a
  charsetEncode (ParameterValue _ lang s) =
    let
      bs = T.encodeUtf8 s
      cs = if B.all (< 0x80) bs then Nothing else Just "utf-8"
    in ParameterValue cs lang bs

getParameter :: CI B.ByteString -> RawParameters -> Maybe EncodedParameterValue
getParameter k m = do
  InitialSection cont enc s <- initialSection k m
  (cs, lang, v0) <-
    either (const Nothing) Just $ parseOnly (parseInitialValue enc) s
  let
    sect0 = OtherSection enc v0
    otherSects i = maybe [] (: otherSects (i + 1)) (otherSection k i m)
    sects = case cont of
      NotContinued -> [sect0]
      Continued -> sect0 : otherSects 1
  ParameterValue cs lang . fold <$> traverse decode sects
  where
    parseInitialValue NotEncoded =
      (Nothing, Nothing, ) <$> takeByteString
    parseInitialValue Encoded =
      (,,) <$> charsetOrLang <*> charsetOrLang <*> takeByteString
    charsetOrLang = optional (ci (takeWhile1 (/= '\''))) <* char8 '\''

    decode (OtherSection enc s) = case enc of
      NotEncoded -> pure s
      Encoded -> decodePercent s


decodePercent :: B.ByteString -> Maybe B.ByteString
decodePercent (B.PS sfp soff slen) = unsafeDupablePerformIO $ do
  -- Length of decoded string is not yet known, but it cannot be
  -- longer than input, and is likely to be not much shorter.
  -- Therefore allocate slen bytes and only use as much as we need.
  dfp <- B.mallocByteString slen

  result <- withForeignPtr dfp $ \dptr ->
    withForeignPtr sfp $ \sptr -> do
      let
        slimit = sptr `plusPtr` (soff + slen)
        fill !dp !sp
          | sp >= slimit = pure $ Just (dp `minusPtr` dptr)
          | otherwise = do
            c <- peek sp
            case (c :: Word8) of
              37 {- % -}
                | sp `plusPtr` 2 >= slimit -> pure Nothing
                  -- reached end of input during '=' decoding
                | otherwise -> do
                    c1 <- peekByteOff sp 1
                    c2 <- peekByteOff sp 2
                    maybe
                      (pure Nothing) -- invalid hex sequence
                      (\(hi,lo) -> do
                        poke dp (hi * 16 + lo)
                        fill (dp `plusPtr` 1) (sp `plusPtr` 3) )
                      ((,) <$> parseHex c1 <*> parseHex c2)
              _ ->
                poke dp c *> fill (dp `plusPtr` 1) (sp `plusPtr` 1)

      fill dptr (sptr `plusPtr` soff)
  pure $ B.PS dfp 0 <$> result

data ParameterEncoding = Plain | Quoted | Extended
  deriving (Eq, Ord, Bounded)

-- | Given a requested encoding and a string, return an encoded
-- string along with the actual encoding used.
--
-- The requested encoding will be used when it is capable of
-- encoding the string, otherwise the first capable encoding
-- is used.
--
extEncode :: ParameterEncoding -> B.ByteString -> (ParameterEncoding, B.ByteString)
extEncode encReq s@(B.PS sfp soff slen) = (enc, d)
  where
  -- regular parameter:
  --  value := token / quoted-string   (RFC 2045)
  --  token := 1*<any (US-ASCII) CHAR except SPACE, CTLs, or tspecials>
  --  tspecials :=  "(" / ")" / "<" / ">" / "@" /
  --                "," / ";" / ":" / "\" / <">
  --                "/" / "[" / "]" / "?" / "="
  --
  -- extended-parameter:
  --  attribute-char := <any (US-ASCII) CHAR except SPACE, CTLs, "*", "'", "%", or tspecials>
  --  extended-other-values := *(ext-octet / attribute-char)
  --  ext-octet := "%" 2(DIGIT / "A" / "B" / "C" / "D" / "E" / "F")
  --
  isTspecial = (`B.elem` "()<>@,;:\\\"/[]?=")
  isAttrChar c = isVchar c && c `B.notElem` "*'%" && not (isTspecial c)
  numEncChars c = if isAttrChar c then 1 else 3  -- conservative estimate of bytes
                                                 -- needed to encode char
  charEncoding c
    | isAttrChar c = Plain
    | isVchar c || c == 0x20 || c == 0x09 = Quoted
    | otherwise = Extended
  charInfo c = (Sum (numEncChars c), Max (charEncoding c))
  (Sum dlenMax, encCap) = foldMap charInfo $ B.unpack s
  enc
    | B.null s = Quoted  -- Plain cannot encode empty string
    | otherwise = getMax (Max encReq <> encCap)

  -- poke the char (possibly encoded) and return updated dest ptr
  poke' ptr c = case enc of
    Plain -> poke ptr c $> ptr `plusPtr` 1
    Quoted
      | isQtext c -> poke ptr c $> ptr `plusPtr` 1
      | otherwise -> do
          poke ptr 0x5c -- backslash
          poke (ptr `plusPtr` 1) c
          pure (ptr `plusPtr` 2)
    Extended
      | isAttrChar c -> poke ptr c $> ptr `plusPtr` 1
      | otherwise -> do
          let (hi, lo) = hexEncode c
          poke ptr 37 {- % -}
          poke (ptr `plusPtr` 1) hi
          poke (ptr `plusPtr` 2) lo
          pure (ptr `plusPtr` 3)

  d = unsafeDupablePerformIO $ do
    dfp <- B.mallocByteString dlenMax
    dlen <- withForeignPtr dfp $ \dptr ->
      withForeignPtr sfp $ \sptr -> do
        let
          slimit = sptr `plusPtr` (soff + slen)
          fill !sp !dp
            | sp >= slimit = pure (dp `minusPtr` dptr)
            | otherwise = peek sp >>= poke' dp >>= fill (sp `plusPtr` 1)
        fill sptr dptr
    pure $ B.PS dfp 0 dlen

-- | Types that have 'Parameters'
class HasParameters a where
  parameters :: Lens' a Parameters

instance HasParameters Parameters where
  parameters = id

-- Access the 'Parameters' as a @[(CI B.ByteString, B.ByteString)]@
parameterList :: HasParameters a => Lens' a RawParameters
parameterList = parameters . coerced

-- | Access parameter value.  Continuations, encoding and charset
-- are processed.
--
parameter
  :: HasParameters a
  => CI B.ByteString -> Lens' a (Maybe EncodedParameterValue)
parameter k = parameters . at k

-- | Raw parameter.  The key is used as-is.  No processing of
-- continuations, encoding or charset is performed.
--
rawParameter :: HasParameters a => CI B.ByteString -> Traversal' a B.ByteString
rawParameter k = parameters . paramiso . traversed . filtered ((k ==) . fst) . _2
