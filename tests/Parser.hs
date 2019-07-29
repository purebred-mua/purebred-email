-- This file is part of purebred-email
-- Copyright (C) 2019  Fraser Tweedale
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

module Parser where

import Data.Attoparsec.ByteString.Lazy as AL
import Data.ByteString as B
import Data.ByteString.Lazy as L
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck

import Data.RFC5322.Internal (takeTillString)

tests :: TestTree
tests = testGroup "Parser tests"
  [ testProperty "takeTillString" $ \h l c r ->
      let
        s = h <> l <> c <> r
        s' = L.fromChunks $ B.foldr ((:) . B.singleton) [] s
      in
        not (c `B.isInfixOf` l) ==>
          case AL.parse (AL.string h *> takeTillString c) s' of
            AL.Done r' l' -> r' == L.fromStrict r && l' == l
            _ -> False
  ]
