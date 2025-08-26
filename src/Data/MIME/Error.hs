-- This file is part of purebred-email
-- Copyright (C) 2018-2021  Fraser Tweedale
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

module Data.MIME.Error where

import Control.Lens (Prism', prism')

import Data.MIME.Charset
import Data.MIME.TransferEncoding


-- | Transfer or character encoding errors
--
data EncodingError
  = TransferEncodingError TransferEncodingError
  | CharsetError CharsetError
  deriving (Show)

class AsEncodingError s where
  _EncodingError :: Prism' s EncodingError
  _EncodingErrorTransferEncodingError :: Prism' s TransferEncodingError
  _EncodingErrorCharsetError :: Prism' s CharsetError

  _EncodingErrorTransferEncodingError = _EncodingError . _TransferEncodingError
  _EncodingErrorCharsetError = _EncodingError . _CharsetError

instance AsEncodingError EncodingError where
  _EncodingError = id
  _EncodingErrorTransferEncodingError = prism' TransferEncodingError $ \case
    TransferEncodingError e -> Just e ; _ -> Nothing
  _EncodingErrorCharsetError = prism' CharsetError $ \case
    CharsetError e -> Just e ; _ -> Nothing

instance AsCharsetError EncodingError where
  _CharsetError = _EncodingErrorCharsetError

instance AsTransferEncodingError EncodingError where
  _TransferEncodingError = _EncodingErrorTransferEncodingError
