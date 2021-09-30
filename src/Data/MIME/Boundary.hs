-- This file is part of purebred-email
-- Copyright (C) 2021  Fraser Tweedale
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

module Data.MIME.Boundary
  (
    Boundary
  , unBoundary
  , makeBoundary
  ) where

import Control.Monad (replicateM)

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.ByteString.Char8 as C8
import System.Random.Stateful

-- | MIME boundary.  Use 'makeBoundary' to construct, and 'unBoundary'
-- to unwrap.
--
-- Use the 'Uniform' instance to generate a random @Boundary@ to use
-- when constructing messages.  For example:
--
-- @
-- 'getStdRandom' 'uniform' :: MonadIO m =>  m Boundary
-- 'getStdRandom' 'uniform' ::              IO Boundary
-- @
--
newtype Boundary = Boundary B.ByteString
  deriving (Eq, Show)

unBoundary :: Boundary -> B.ByteString
unBoundary (Boundary s) = s

-- Boundary smart constructor that checks validity
makeBoundary :: B.ByteString -> Either B.ByteString Boundary
makeBoundary s
  | B.null s                    = Left s
  | B.length s > 70             = Left s
  | B.any (not . validBchar) s  = Left s
  | B.last s == 0x20            = Left s
  | otherwise                   = Right $ Boundary s
  where
    validBchar c =
      c >= 0x2c && c <= 0x3a -- ',', '-', '.', '/', '0'..'9', ':'
      || c >= 0x41 && c <= 0x5a -- 'A'..'Z'
      || c >= 0x61 && c <= 0x7a -- 'a'..'z'
      || c >= 0x27 && c <= 0x29 -- '\'', '(', ')'
      || c == 0x2b {- '+' -}
      || c == 0x5f {- '_' -}
      || c == 0x3d {- '=' -}
      || c == 0x3f {- '?' -}
      || c == 0x20 {- ' ' -}

-- | Generate a random 'Boundary'
genBoundary :: (StatefulGen g m) => g -> m Boundary
genBoundary g = do
  let
    blen = 64
    bchars = C8.pack $ ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z'] <> "'()+_,-./:=?"
  chars <-
    replicateM blen $ B.index bchars <$> uniformRM (0, B.length bchars - 1) g
  pure . Boundary $ B.unsafePackLenBytes blen chars

instance Uniform Boundary where
  uniformM = genBoundary
