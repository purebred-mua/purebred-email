{-# LANGUAGE RankNTypes #-}

module Data.MIME.Types
  (
    ContentTransferEncoding
  , EncodedWordEncoding
  ) where

import Control.Lens (APrism')
import qualified Data.ByteString as B

type ContentTransferEncoding = APrism' B.ByteString B.ByteString

type EncodedWordEncoding = APrism' B.ByteString B.ByteString
