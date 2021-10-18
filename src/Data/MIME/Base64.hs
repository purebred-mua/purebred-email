-- This file is part of purebred-email
-- Copyright (C) 2017-2020  Fraser Tweedale
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

{- |

Implementation of Base64 Content-Transfer-Encoding.

<https://tools.ietf.org/html/rfc2045#section-6.8>

-}
module Data.MIME.Base64
  (
    b
  , contentTransferEncodeBase64
  , contentTransferEncodingBase64
  ) where

import Control.Lens (prism')
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.Lazy as L64
import Data.Word (Word8)

import Data.MIME.Types (ContentTransferEncoding)



isBase64Char :: Word8 -> Bool
isBase64Char c =
  (c >= 0x41 && c <= 0x5a)  -- A-Z
  || (c >= 0x61 && c <= 0x7a) -- a-z
  || (c >= 0x30 && c <= 0x39) -- 0-9
  || c == 43 -- +
  || c == 47 -- /
  || c == 61 -- =

{-

Notes about encoding requirements:

- The encoded output stream must be represented in lines of no more
  than 76 characters each.

-}
contentTransferEncodeBase64 :: B.ByteString -> B.ByteString
contentTransferEncodeBase64 = L.toStrict . wrap . L64.encode . L.fromStrict
  where
  wrap s = case L.splitAt 76 s of
    (l, "") -> l
    (l, s') -> l <> "\r\n" <> wrap s'

{-

Notes about decoding requirements:

- All line breaks or other characters not found in Table 1 must be
  ignored by decoding software.

- In base64 data, characters other than those in Table 1, line breaks,
  and other white space probably indicate a transmission error, about
  which a warning message or even a message rejection might be
  appropriate under some circumstances.

-}
contentTransferDecodeBase64 :: B.ByteString -> Either String B.ByteString
contentTransferDecodeBase64 = B64.decode . B.filter isBase64Char

contentTransferEncodingBase64 :: ContentTransferEncoding
contentTransferEncodingBase64 = prism'
  contentTransferEncodeBase64
  (either (const Nothing) Just . contentTransferDecodeBase64)

b :: ContentTransferEncoding
b = prism'
  B64.encode
  (either (const Nothing) Just . contentTransferDecodeBase64)
