{-# LANGUAGE OverloadedStrings #-}

{- |

MIME character sets.

Recognised charsets:

* @us-ascii@ / @iso646-us@
* @utf-8@
* @iso-8859-1@

-}
module Data.MIME.Charset
  ( Charset
  , charsets
  , lookupCharset
  , decodeLenient
  ) where

import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

type Charset = B.ByteString -> T.Text  -- eventually we might want a prism

charsets :: [(CI.CI B.ByteString, Charset)]
charsets =
  [ ("us-ascii", us_ascii)
  , ("utf-8", utf_8)
  , ("iso-8859-1", iso_8859_1)

  -- uncommon aliases
  , ("ISO646-US", us_ascii)
  , ("ANSI_X3.4-1968", us_ascii)

  -- , ("iso-8859-2", ...)
  -- , ("iso-8859-15", ...)
  -- , ("iso-2022-jp", ...)    (common)
  -- , ("windows-1252", ...)   (common)
  -- , ("windows-1256", ...)
  -- , ("cp1252", ...)         (same as windows-1256?)
  -- , ("big5", ...)           (common)
  -- , ("euc-kr", ...)
  -- , ("cp932", ...)
  -- , ("gb2312", ...)         (Chinese)
  ]

us_ascii, utf_8, iso_8859_1 :: Charset
us_ascii = decodeLenient
utf_8 = decodeLenient
iso_8859_1 = T.decodeLatin1

-- | Look up a character set.
--
lookupCharset :: CI.CI B.ByteString -> Maybe Charset
lookupCharset k = lookup k charsets

-- | Decode as UTF-8, replacing invalid sequences with placeholders.
--
decodeLenient :: B.ByteString -> T.Text
decodeLenient = T.decodeUtf8With T.lenientDecode
