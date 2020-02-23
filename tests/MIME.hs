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

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module MIME where

import Control.Exception (ErrorCall, evaluate, try)
import Control.Monad ((<=<), void)
import Data.Bifunctor (first)
import Data.Char (toUpper)
import Data.Either (isLeft)
import Data.List.NonEmpty (fromList)
import Data.String (fromString)
import Data.Time.Clock (UTCTime)

import Control.Lens
import qualified Data.ByteString as B
import qualified Data.Text as T

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), Assertion, assertBool, assertFailure, testCase)
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances ()

import Data.MIME
import Data.MIME.Charset

unittests :: TestTree
unittests = testGroup "MIME tests"
  [ testContentDisposition
  , testParse
  , testOptics
  , testParameterValueOverloadedStrings
  , testContentTypeOverloadedStrings
  ]

testContentDisposition :: TestTree
testContentDisposition =
  testGroup "content disposition"
    [ testCase "read empty (plain; should fail)" $
        preview lFilename
        (Message (Headers [("Content-Disposition", "attachment; filename=")]) (Part ""))
        @?= Nothing
    , testCase "read empty (quoted)" $
        preview lFilename
        (Message (Headers [("Content-Disposition", "attachment; filename=\"\"")]) (Part ""))
        @?= Just ""
    , testCase "read empty (extended)" $
        preview lFilename
        (Message (Headers [("Content-Disposition", "attachment; filename*=''")]) (Part ""))
        @?= Just ""
    , testCase "read plain" $
        preview lFilename
        (Message (Headers [("Content-Disposition", "attachment; filename=foo.pdf")]) (Part ""))
        @?= Just "foo.pdf"
    , testCase "read quoted" $
        preview lFilename
        (Message (Headers [("Content-Disposition", "attachment; filename=\"/tmp/foo.pdf\"")]) (Part ""))
        @?= Just "/tmp/foo.pdf"
    , testCase "set quoted (empty string)" $
        (view headers . set lFilename "")
        (Message (Headers [("Content-Disposition", "attachment; filename=foo.pdf")]) (Part ""))
        @?= Headers [("Content-Disposition", "attachment; filename=\"\"")]
    , testCase "set quoted (space char)" $
        (view headers . set lFilename "hello world.txt")
        (Message (Headers [("Content-Disposition", "attachment; filename=foo.pdf")]) (Part ""))
        @?= Headers [("Content-Disposition", "attachment; filename=\"hello\\ world.txt\"")]
    , testCase "set quoted (backslash char)" $
        (view headers . set lFilename "hello\\world.txt")
        (Message (Headers [("Content-Disposition", "attachment; filename=foo.pdf")]) (Part ""))
        @?= Headers [("Content-Disposition", "attachment; filename=\"hello\\\\world.txt\"")]
    , testCase "set quoted (=)" $
        (view headers . set lFilename "hello=world.txt")
        (Message (Headers [("Content-Disposition", "attachment; filename=foo.pdf")]) (Part ""))
        @?= Headers [("Content-Disposition", "attachment; filename=\"hello=world.txt\"")]
    , testCase "set quoted (\")" $
        (view headers . set lFilename "hello\"world\".txt")
        (Message (Headers [("Content-Disposition", "attachment; filename=foo.pdf")]) (Part ""))
        @?= Headers [("Content-Disposition", "attachment; filename=\"hello\\\"world\\\".txt\"")]
    , testCase "modify plain -> plain" $
        (preview lFilename . over lFilename (T.drop 1))
        (Message (Headers [("Content-Disposition", "attachment; filename=foo.pdf")]) (Part ""))
        @?= Just "oo.pdf"
    , testCase "modify plain -> quoted" $
        (preview lFilename . over lFilename (T.map (\c -> if c == '.' then '\\' else c)))
        (Message (Headers [("Content-Disposition", "attachment; filename=foo.pdf")]) (Part ""))
        @?= Just "foo\\pdf"
    , testCase "modify quoted -> plain" $
        (preview lFilename . over lFilename stripPath)
        (Message (Headers [("Content-Disposition", "attachment; filename=\"/tmp/foo.pdf\"")]) (Part ""))
        @?= Just "foo.pdf"
    , testCase "modify quoted -> quoted" $
        (preview lFilename . over lFilename (T.map toUpper))
        (Message (Headers [("Content-Disposition", "attachment; filename=\"/tmp/foo.pdf\"")]) (Part ""))
        @?= Just "/TMP/FOO.PDF"
    , testCase "set extended (utf-8; raw)" $
        (view headers . set lFilename "hello世界.txt")
        (Message (Headers [("Content-Disposition", "attachment; filename=\"/tmp/foo.pdf\"")]) (Part ""))
        @?= Headers [("Content-Disposition", "attachment; filename*=utf-8''hello%E4%B8%96%E7%95%8C.txt")]
    , testCase "set extended (utf-8; readback)" $
        (preview lFilename . set lFilename "hello世界.txt")
        (Message (Headers [("Content-Disposition", "attachment; filename=\"/tmp/foo.pdf\"")]) (Part ""))
        @?= Just "hello世界.txt"
    , testCase "set extended (us-ascii; charset omitted; raw)" $
        -- control characters will force it to use percent-encoded extended param,
        -- but all chars are in us-ascii so charset should be omitted
        (view headers . set lFilename "new\nline")
        (Message (Headers [("Content-Disposition", "attachment; filename=\"/tmp/foo.pdf\"")]) (Part ""))
        @?= Headers [("Content-Disposition", "attachment; filename*=''new%0Aline")]
    , testCase "set extended (us-ascii; charset omitted; readback)" $
        -- control characters will force it to use percent-encoded extended param,
        -- but all chars are in us-ascii so charset should be omitted
        (preview lFilename . set lFilename "new\nline")
        (Message (Headers [("Content-Disposition", "attachment; filename=\"/tmp/foo.pdf\"")]) (Part ""))
        @?= Just "new\nline"
    , testProperty "filename round-trip" $ \s ->
        (preview lFilename . set lFilename s)
        (Message (Headers [("Content-Disposition", "attachment; filename=foo.pdf")]) (Part ""))
        == Just s
    , testCase "unset multiple filenames" $
        set
          (attachments . headers . contentDisposition . traversed . filenameParameter)
          Nothing
        (Message (Headers []) (Multipart . fromList $
          [ Message (Headers [("Content-Disposition", "inline; filename=msg.txt")]) (Part "")
          , Message (Headers [("Content-Disposition", "attachment; filename=foo.pdf")]) (Part "")
          , Message (Headers [("Content-Disposition", "attachment; filename=bar.pdf")]) (Part "")
          ]
        ))
        @?=
        Message (Headers []) (Multipart . fromList $
          [ Message (Headers [("Content-Disposition", "inline; filename=msg.txt")]) (Part "")
          , Message (Headers [("Content-Disposition", "attachment")]) (Part "")
          , Message (Headers [("Content-Disposition", "attachment")]) (Part "")
          ]
        )
    ]
  where
    lFilename = headers . contentDisposition . traversed . filename defaultCharsets
    stripPath = snd . T.breakOnEnd "/"

testParse :: TestTree
testParse = testGroup "parsing tests"
  [ testCase "nested multipart" $
      testParseFile "test-vectors/nested-multipart.eml"
  ]

testParseFile :: FilePath -> Assertion
testParseFile =
  either assertFailure (void . pure) . parse (message mime) <=< B.readFile

testParameterValueOverloadedStrings :: TestTree
testParameterValueOverloadedStrings = testGroup "ParameterValue IsString instances"
  [ testCase "DecodedParameterValue" $
      let
        -- start with DecodedParameterValue, then round-trip it
        v' = charsetEncode "hello世界" :: EncodedParameterValue
      in
        meh (view (charsetDecoded defaultCharsets) v')
          @?= Right (ParameterValue Nothing Nothing "hello世界")
  , testCase "EncodedParameterValue" $
      let
        -- start with EncodedParameterValue, then decode it
        v = "hello世界" :: EncodedParameterValue
      in
        meh (view (charsetDecoded defaultCharsets) v)
          @?= Right (ParameterValue Nothing Nothing "hello世界")
  ]
  where
    meh :: Either CharsetError b -> Either () b
    meh = first (const ())

testContentTypeOverloadedStrings :: TestTree
testContentTypeOverloadedStrings = testGroup "ContentType fromString"
  [ testCase "no params" $ fromString "foo/bar" @?= ContentType "foo" "bar" mempty
  , testCase "params" $ fromString "foo/bar; baz=quux" @?= ContentType "foo" "bar" (Parameters [("baz", "quux")])
  , testCase "bogus" $
      (isLeft <$> (try . evaluate $ fromString "foo/; baz=quux" :: IO (Either ErrorCall ContentType)))
      >>= assertBool "bogus string throws error"
  ]

testOptics :: TestTree
testOptics = testGroup "optics tests"
  [ testCase "headerDate get valid date" $
      testHeaderDateGet
      "Thu, 4 May 2017 03:08:43 +0000"
      (Just $ read "2017-05-04 03:08:43 UTC")
  , testCase "headerDate get invalid date" $
      testHeaderDateGet
      "Thu, 4 NOTMAY 2017 03:08:43 +0000"
      Nothing
  , testCase "headerDate set" $
      testHeaderDateSet (Just $ read "2017-05-04 03:08:43 UTC")
  , testCase "headerDate unset" $ testHeaderDateSet Nothing
  ]

testHeaderDateGet :: B.ByteString -> Maybe UTCTime -> Assertion
testHeaderDateGet headerStr time =
  view headerDate msg @?= time
  where
    Right msg = parse (message mime) msgStr
    msgStr = "Date: " <> headerStr <> "\n\nbody\n" :: B.ByteString

testHeaderDateSet :: Maybe UTCTime -> Assertion
testHeaderDateSet time =
  view headerDate msg @?= time
  where
    msg = set headerDate time $ createTextPlainMessage "body"
