-- This file is part of purebred-email
-- Copyright (C) 2018  Fraser Tweedale
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
