{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |

Implementation of Quoted-Printable Content-Transfer-Encoding.

<https://tools.ietf.org/html/rfc2045#section-6.7>

-}
module Data.MIME.QuotedPrintable
  (
    contentTransferEncodingQuotedPrintable
  ) where

import Control.Lens (prism')
import Data.Bits ((.&.), shiftR)
import Data.Bool (bool)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.Word (Word8)
import Foreign
  ( Ptr, withForeignPtr, nullPtr, plusPtr, minusPtr
  , peek, peekByteOff, poke
  )
import System.IO.Unsafe (unsafeDupablePerformIO)

import Data.MIME.Types (ContentTransferEncoding)

-- | Whether it is required to encode a character
-- (where that character does not precede EOL).
encodingRequiredNonEOL :: Word8 -> Bool
encodingRequiredNonEOL c = not (
  (c >= 33 && c <= 60)
  || (c >= 62 && c <= 126)
  || c == 9
  || c == 32
  )


-- | Whether it is required to encode a character
-- (where that character does precede EOL).
encodingRequiredEOL :: Word8 -> Bool
encodingRequiredEOL c = not (
  (c >= 33 && c <= 60)
  || (c >= 62 && c <= 126)
  )

-- | Two-pass solution: first determine output length, then
-- do the copy.
-- output length.
contentTransferEncodeQuotedPrintable :: B.ByteString -> B.ByteString
contentTransferEncodeQuotedPrintable s = unsafeDupablePerformIO $ do
  l <- contentTransferEncodeQuotedPrintable'
        (\_ _ -> pure ()) id nullPtr s
  dfp <- B.mallocByteString l
  withForeignPtr dfp $ \dptr ->
    contentTransferEncodeQuotedPrintable'
      poke (B.PS dfp 0) dptr s

contentTransferEncodeQuotedPrintable'
  :: (Ptr Word8 -> Word8 -> IO ()) -- "poke" function
  -> (Int -> r)                    -- "return" function
  -> Ptr Word8
  -- ^ dest pointer; **assumed to be big enough to hold output**.
  -- Can pass a bogus pointer (e.g. nullPtr) if the poke function
  -- ignores its argument; this can be used for a first pass that
  -- just computes the required length.
  -> B.ByteString
  -- ^ input string
  -> IO r
contentTransferEncodeQuotedPrintable' poke' mkResult dptr (B.PS sfp soff slen) =
  fmap mkResult $ withForeignPtr sfp $ \sptr -> do
    let
      slimit = sptr `plusPtr` (soff + slen)

      -- is there a crlf at this location?
      crlf :: Ptr Word8 -> IO Bool
      crlf ptr
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
                    (cAtEOL && encodingRequiredEOL c)
                    || encodingRequiredNonEOL c
                  bytesNeeded = bool 1 3 encodingRequired
                case (col + bytesNeeded >= 76, encodingRequired) of
                  (False, False) ->
                    poke' dp c
                    *> fill (col + bytesNeeded) (dp `plusPtr` bytesNeeded) (sp `plusPtr` 1)
                  (False, True) ->
                    pokeEncoded dp c
                    *> fill (col + bytesNeeded) (dp `plusPtr` bytesNeeded) (sp `plusPtr` 1)
                  (True, False) ->
                    pokeSoftLineBreak dp
                    *> poke' (dp `plusPtr` 3) c
                    *> fill 1 (dp `plusPtr` 4) (sp `plusPtr` 1)
                  (True, True) ->
                    pokeSoftLineBreak dp
                    *> pokeEncoded (dp `plusPtr` 3) c
                    *> fill 3 (dp `plusPtr` 6) (sp `plusPtr` 1)
    fill 0 dptr (sptr `plusPtr` soff)

contentTransferDecodeQuotedPrintable :: B.ByteString -> Either String B.ByteString
contentTransferDecodeQuotedPrintable (B.PS sfp soff slen) = unsafeDupablePerformIO $ do
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
              _ ->
                -- assume that the char is valid here;
                -- copy to dp and continue
                poke dp c *> fill (dp `plusPtr` 1) (sp `plusPtr` 1)
      fill dptr (sptr `plusPtr` soff)
  pure $ B.PS dfp 0 <$> result

contentTransferEncodingQuotedPrintable :: ContentTransferEncoding
contentTransferEncodingQuotedPrintable = prism'
  contentTransferEncodeQuotedPrintable
  (either (const Nothing) Just . contentTransferDecodeQuotedPrintable)

{-
  (1)   An "=" followed by two hexadecimal digits, one or both
        of which are lowercase letters in "abcdef", is formally
        illegal. A robust implementation might choose to
        recognize them as the corresponding uppercase letters.
-}
parseHex :: Word8 -> Maybe Word8
parseHex c = do
  let
    -- to upper
    c' = if c >= 0x61 && c <= 0x7a then c - 0x20 else c
  fromIntegral <$> B.findIndex (== c') hexAlphabet

hexEncode :: Word8 -> (Word8, Word8)
hexEncode c =
  let
    lkup i = B.index hexAlphabet (fromIntegral i)
  in
    ( lkup (c `shiftR` 4)
    , lkup (c .&. 0x0f)
    )

hexAlphabet :: B.ByteString
hexAlphabet = "0123456789ABCDEF"
