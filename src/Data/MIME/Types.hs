{-# LANGUAGE RankNTypes #-}

module Data.MIME.Types
  (
    ContentTransferEncoding
  ) where

import Control.Lens (Prism')
import qualified Data.ByteString as B

type ContentTransferEncoding = Prism' B.ByteString B.ByteString
