{-# LANGUAGE RankNTypes #-}

module Data.MIME.Types
  (
    ContentTransferEncoding
  , EncodedWordEncoding
  , Encoding(..)
  ) where

import Control.Lens (APrism')
import qualified Data.ByteString as B

type ContentTransferEncoding = APrism' B.ByteString B.ByteString
data Encoding = None | Base64

type EncodedWordEncoding = APrism' B.ByteString B.ByteString
