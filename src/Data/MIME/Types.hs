{-# LANGUAGE RankNTypes #-}

module Data.MIME.Types
  (
    ContentTransferEncoding
  ) where

import Control.Lens (APrism')
import qualified Data.ByteString as B

type ContentTransferEncoding = APrism' B.ByteString B.ByteString
