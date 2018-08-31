{-# LANGUAGE BangPatterns #-}
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
    Parameters
  , parameter
  , rawParameter

  , ParameterValue
  , value

  , HasParameters(..)
  ) where

import Control.Applicative ((<|>), optional)
import Data.Foldable (fold)
import Data.Semigroup ((<>))
import Data.Word (Word8)
import Foreign (withForeignPtr, plusPtr, minusPtr, peek, peekByteOff, poke)
import System.IO.Unsafe (unsafeDupablePerformIO)

import Control.Lens (Fold, Lens, Lens', _2, filtered, folded, set, to, view)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Char8 as C
import Data.CaseInsensitive (CI, mk)
import qualified Data.Text as T

import Data.MIME.Charset
import Data.MIME.Internal
import Data.RFC5322.Internal (ci)

type Parameters = [(CI B.ByteString, B.ByteString)]

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
  -> Parameters
  -> Maybe InitialSection
initialSection k m =
  InitialSection NotContinued NotEncoded <$> lookup k m
  <|> InitialSection Continued NotEncoded <$> lookup (k <> "*0") m
  <|> InitialSection NotContinued Encoded <$> lookup (k <> "*") m
  <|> InitialSection Continued Encoded <$> lookup (k <> "*0*") m

otherSection
  :: CI B.ByteString
  -> Int
  -> Parameters
  -> Maybe OtherSection
otherSection k i m =
  OtherSection NotEncoded <$> lookup (k <> "*" <> i') m
  <|> OtherSection Encoded <$> lookup (k <> "*" <> i' <> "*") m
  where
    i' = mk $ C.pack (show i)

data ParameterValue a = ParameterValue
  (Maybe (CI B.ByteString))  -- charset
  (Maybe (CI B.ByteString))  -- language
  a                          -- value

value :: Lens (ParameterValue a) (ParameterValue b) a b
value f (ParameterValue a b c) = ParameterValue a b <$> f c


-- | The default charset @us-ascii@ is implied by the abstract of
-- RFC 2231 which states: /This memo defines … a means to specify
-- parameter values in character sets other than US-ASCII/.
--
instance HasCharset (ParameterValue B.ByteString) where
  type Decoded (ParameterValue B.ByteString) = ParameterValue T.Text
  charsetName = to $ \(ParameterValue name _ _) -> name <|> Just "us-ascii"
  charsetData = value
  charsetDecoded = to $ \a -> (\t -> set value t a) <$> view charsetText a

getParameter :: CI B.ByteString -> Parameters -> Maybe (ParameterValue B.ByteString)
getParameter k m = do
  InitialSection cont enc s <- initialSection k m
  (charset, lang, v0) <-
    either (const Nothing) Just $ parseOnly (parseInitialValue enc) s
  let
    sect0 = OtherSection enc v0
    otherSects i = maybe [] (: otherSects (i + 1)) (otherSection k i m)
    sects = case cont of
      NotContinued -> [sect0]
      Continued -> sect0 : otherSects 1
  ParameterValue charset lang . fold <$> traverse decode sects
  where
    parseInitialValue NotEncoded =
      (Nothing, Nothing, ) <$> takeByteString
    parseInitialValue Encoded =
      (,,) <$> charsetOrLang <*> charsetOrLang <*> takeByteString
    charsetOrLang = optional (ci (takeWhile1 (/= '\''))) <* char8 '\''

    decode (OtherSection enc s) = case enc of
      NotEncoded -> pure s
      Encoded -> decodePercent s


-- | Get parameter value.  Continuations, encoding and charset
-- are processed.
--
parameter :: CI B.ByteString -> Fold Parameters (ParameterValue B.ByteString)
parameter k = to (getParameter k) . folded

-- | Raw parameter.  The key is used as-is.  No processing of
-- continuations, encoding or charset is performed.
--
rawParameter :: CI B.ByteString -> Fold Parameters B.ByteString
rawParameter k = folded . filtered ((k ==) . fst) . _2


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

class HasParameters a where
  parameters :: Lens' a Parameters
