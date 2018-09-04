-- This file is part of purebred-email
-- Copyright (C) 2018  Fraser Tweedale
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

module MIME where

import Control.Lens

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Data.MIME

unittests :: TestTree
unittests =
  testGroup "content disposition"
    [
      testCase "simple" $
      preview (headers . contentDisposition . filename)
      (Message [("Content-Disposition", "attachment; filename=foo.pdf")] (Part "foo"))
      @?= Just "foo.pdf"
    ]
