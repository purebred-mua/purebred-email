{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |

Implementation of Quoted-Printable Content-Transfer-Encoding.

<https://tools.ietf.org/html/rfc2045#section-6.7>

-}
module Data.MIME.QuotedPrintable
  (
    contentTransferEncodingQuotedPrintable
  , q
  , QuotedPrintableMode(..)
  , encodingRequiredEOL
  , encodingRequiredNonEOL
  ) where

import Control.Lens (APrism', prism')
import Data.Bool (bool)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.Word (Word8)
import Foreign
  ( Ptr, withForeignPtr, nullPtr, plusPtr, minusPtr
  , peek, peekByteOff, poke
  )
import System.IO.Unsafe (unsafeDupablePerformIO)

import Data.MIME.Internal
import Data.MIME.Types

data QuotedPrintableMode = QuotedPrintable | Q
  deriving (Eq)

-- | Whether it is required to encode a character
-- (where that character does not precede EOL).
encodingRequiredNonEOL :: QuotedPrintableMode -> Word8 -> Bool
encodingRequiredNonEOL mode c =
  (c < 32 {- ' ' -} && c /= 9 {- \t -})
  || c == 61 {- = -}
  || c >= 127
  || mode == Q && (c == 95 {- _ -} || c == 9 {- \t -} || c == 63 {- ? -})


-- | Whether it is required to encode a character
-- (where that character does precede EOL).
encodingRequiredEOL :: QuotedPrintableMode -> Word8 -> Bool
encodingRequiredEOL mode c = not (
  (c >= 33 && c <= 60)
  || (c >= 62 && c <= 126)
  ) || (mode == Q && c == 95 {- underscore -})

-- | Two-pass solution: first determine output length, then
-- do the copy.
-- output length.
encodeQuotedPrintable :: QuotedPrintableMode -> B.ByteString -> B.ByteString
encodeQuotedPrintable mode s = unsafeDupablePerformIO $ do
  l <- encodeQuotedPrintable' mode
        (\_ _ -> pure ()) id nullPtr s
  dfp <- B.mallocByteString l
  withForeignPtr dfp $ \dptr ->
    encodeQuotedPrintable' mode
      poke (B.PS dfp 0) dptr s

encodeQuotedPrintable'
  :: QuotedPrintableMode
  -> (Ptr Word8 -> Word8 -> IO ()) -- "poke" function
  -> (Int -> r)                    -- "return" function
  -> Ptr Word8
  -- ^ dest pointer; **assumed to be big enough to hold output**.
  -- Can pass a bogus pointer (e.g. nullPtr) if the poke function
  -- ignores its argument; this can be used for a first pass that
  -- just computes the required length.
  -> B.ByteString
  -- ^ input string
  -> IO r
encodeQuotedPrintable' mode poke' mkResult dptr (B.PS sfp soff slen) =
  fmap mkResult $ withForeignPtr sfp $ \sptr -> do
    let
      slimit = sptr `plusPtr` (soff + slen)

      -- is there a crlf at this location?
      crlf :: Ptr Word8 -> IO Bool
      crlf ptr
        | mode == Q = pure False  -- always encode CRLF in 'Q' mode
        | ptr `plusPtr` 1 >= slimit = pure False
        | otherwise = do
          c1 <- peek ptr
          c2 <- peekByteOff ptr 1
          pure $ (c1 :: Word8) == 13 && (c2 :: Word8) == 10

      pokeHardLineBreak ptr =
        poke' ptr 13 *> poke' (ptr `plusPtr` 1) 10

      pokeSoftLineBreak ptr =
        poke' ptr 61 {- = -} *> pokeHardLineBreak (ptr `plusPtr` 1)

      pokeEncoded ptr c =
        let (hi, lo) = hexEncode c
        in poke' ptr 61 {- = -}
          *> poke' (ptr `plusPtr` 1) hi
          *> poke' (ptr `plusPtr` 2) lo

      mapChar 32 {- ' ' -} | mode == Q = 95 {- _ -}
      mapChar c = c

      -- Do not wrap lines in Q mode.  This is not correct,
      -- but encoded-word wrapping needs separate encoded-words
      -- including the leading =?... and trailing ?=
      wrapLimit = if mode == Q then maxBound else 76

      fill col !dp !sp
        | sp >= slimit = pure $ dp `minusPtr` dptr
        | otherwise = do
            atEOL <- crlf sp
            if atEOL
              then pokeHardLineBreak dp
                    *> fill 0 (dp `plusPtr` 2) (sp `plusPtr` 2)
              else do
                c <- peek sp
                cAtEOL <- crlf (sp `plusPtr` 1)
                let
                  encodingRequired =
                    (cAtEOL && encodingRequiredEOL mode c)
                    || encodingRequiredNonEOL mode c
                  bytesNeeded = bool 1 3 encodingRequired
                  c' = mapChar c
                case (col + bytesNeeded >= wrapLimit, encodingRequired) of
                  (False, False) ->
                    poke' dp c'
                    *> fill (col + bytesNeeded) (dp `plusPtr` bytesNeeded) (sp `plusPtr` 1)
                  (False, True) ->
                    pokeEncoded dp c'
                    *> fill (col + bytesNeeded) (dp `plusPtr` bytesNeeded) (sp `plusPtr` 1)
                  (True, False) ->
                    pokeSoftLineBreak dp
                    *> poke' (dp `plusPtr` 3) c'
                    *> fill 1 (dp `plusPtr` 4) (sp `plusPtr` 1)
                  (True, True) ->
                    pokeSoftLineBreak dp
                    *> pokeEncoded (dp `plusPtr` 3) c'
                    *> fill 3 (dp `plusPtr` 6) (sp `plusPtr` 1)
    fill 0 dptr (sptr `plusPtr` soff)

decodeQuotedPrintable :: QuotedPrintableMode -> B.ByteString -> Either String B.ByteString
decodeQuotedPrintable mode (B.PS sfp soff slen) = unsafeDupablePerformIO $ do
  -- Precise length of decoded string is not yet known, but
  -- it cannot be longer than input, and is likely to be not
  -- much shorter.  Therefore allocate slen bytes and only
  -- use as much as we need.
  dfp <- B.mallocByteString slen

  result <- withForeignPtr dfp $ \dptr ->
    withForeignPtr sfp $ \sptr -> do
      let
        slimit = sptr `plusPtr` (soff + slen)
        fill !dp !sp
          | sp >= slimit = pure $ Right (dp `minusPtr` dptr)
          | otherwise = do
            c <- peek sp
            case (c :: Word8) of
              61 {- = -} ->
                -- NOTE: strictly, only =\r\n is a valid soft line
                -- break, but we accept =\n as well.
                if sp `plusPtr` 1 >= slimit
                  then pure $ Left "reached end of input during '=' decoding"
                  else do
                    c1 <- peekByteOff sp 1
                    case c1 of
                      10 -> fill dp (sp `plusPtr` 2) -- soft line break (=\n)
                      _ ->
                        if sp `plusPtr` 2 >= slimit
                          then pure $ Left "reached end of input during '=' decoding"
                          else do
                            c2 <- peekByteOff sp 2
                            case (c1, c2) of
                              (13, 10) {- CRLF -} ->
                                -- Soft Line Break (=\r\n)
                                fill dp (sp `plusPtr` 3)
                              _ ->
                                maybe
                                  (pure $ Left "invalid hex sequence")
                                  (\(hi,lo) -> do
                                    poke dp (hi * 16 + lo)
                                    fill (dp `plusPtr` 1) (sp `plusPtr` 3) )
                                  ((,) <$> parseHex c1 <*> parseHex c2)

              -- otherwise assume that the char is valid and copy it to dst

              95 {- _ -} | mode == Q ->
                poke dp 32 {- ' ' -} *> fill (dp `plusPtr` 1) (sp `plusPtr` 1)

              32 {- ' ' -} | mode == Q ->
                pure $ Left "space cannot appear in 'Q' encoding"

              _ ->
                poke dp c *> fill (dp `plusPtr` 1) (sp `plusPtr` 1)

      fill dptr (sptr `plusPtr` soff)
  pure $ B.PS dfp 0 <$> result

mkPrism :: QuotedPrintableMode -> APrism' B.ByteString B.ByteString
mkPrism mode = prism'
  (encodeQuotedPrintable mode)
  (either (const Nothing) Just . decodeQuotedPrintable mode)

contentTransferEncodingQuotedPrintable :: ContentTransferEncoding
contentTransferEncodingQuotedPrintable = mkPrism QuotedPrintable

q :: EncodedWordEncoding
q = mkPrism Q
