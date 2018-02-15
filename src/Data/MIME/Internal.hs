{-# LANGUAGE OverloadedStrings #-}

module Data.MIME.Internal
  (
    hexEncode
  , parseHex
  ) where

import Data.Bits ((.&.), shiftR)
import Data.Word (Word8)

import qualified Data.ByteString as B

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

hexAlphabet :: B.ByteString
hexAlphabet = "0123456789ABCDEF"

hexEncode :: Word8 -> (Word8, Word8)
hexEncode c =
  let
    lkup i = B.index hexAlphabet (fromIntegral i)
  in
    ( lkup (c `shiftR` 4)
    , lkup (c .&. 0x0f)
    )
