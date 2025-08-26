-- This file is part of purebred-email
-- Copyright (C) 2017-2019  Fraser Tweedale
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

module Data.MIME.Types
  (
    ContentTransferEncoding
  , EncodedWordEncoding
  ) where

import Control.Lens (APrism')
import qualified Data.ByteString as B

type ContentTransferEncoding = APrism' B.ByteString B.ByteString

type EncodedWordEncoding = APrism' B.ByteString B.ByteString
