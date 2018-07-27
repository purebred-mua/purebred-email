{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Generator where

import Test.Tasty
import Test.QuickCheck.Instances ()
import Test.Tasty.HUnit ((@=?), testCase)
import Control.Lens (over)

import qualified Data.CaseInsensitive as CI

import Data.MIME


properties :: TestTree
properties =
    testGroup
        "message rendering tests"
        [ renderSimpleMailSuccessfully
        , rendersMultiPartSuccessfully
        , exampleMailsParseSuccessfully
        ]

renderSimpleMailSuccessfully :: TestTree
renderSimpleMailSuccessfully = testCase "renders simple mail" (expected @=? renderMail simpleMail)
  where
    expected = "Subject: Hello there\n\
               \MIME-Version: 1.0\n\
               \Content-Type: text/plain\n\
               \Content-Disposition: inline; charset=\"utf-8\"\n\
               \Content-Transfer-Encoding: 7bit\n\n\
               \This is a simple mail."

rendersMultiPartSuccessfully :: TestTree
rendersMultiPartSuccessfully = testCase "renders simple, multi-part mail" (expected @=? renderMail multiPartMail)
  where
    expected = "Content-Type: multipart/mixed; boundary=asdf\n\
               \MIME-Version: 1.0\n\
               \Subject: Hello there\n\n\
               \--asdf\n\
               \MIME-Version: 1.0\n\
               \Content-Type: text/plain\n\
               \Content-Disposition: inline; charset=\"utf-8\"\n\
               \Content-Transfer-Encoding: 7bit\n\n\
               \This is a simple mail.\n\
               \--asdf\n\
               \MIME-Version: 1.0\n\
               \Content-Type: application/octet-stream\n\
               \Content-Disposition: attachment; filename=\"foo.bin\"\n\
               \Content-Transfer-Encoding: base64\n\n\
               \ZmlsZUNvbnRlbnRzQVNERg==\r\n\n\
               \--asdf--\n"

exampleMailsParseSuccessfully :: TestTree
exampleMailsParseSuccessfully =
    testGroup "example test mails can be round tripped" $
    (\(desc,m) ->
          testCase desc (Right m @=? parse (message mime) (renderMail m))) <$>
    inputs
  where
    inputs = [("simple mail", simpleMail), ("multi part mail", multiPartMail)]

simpleMail :: MIMEMessage
simpleMail =
    let m = createTextPlainMessage "This is a simple mail."
    in over headers ((CI.mk "Subject", "Hello there") :) m

multiPartMail :: MIMEMessage
multiPartMail =
    let hdrs = [(CI.mk "Subject", "Hello there")]
        p = createTextPlainMessage "This is a simple mail."
        a = createAttachment
                contentTypeApplicationOctetStream
                "foo.bin"
                "fileContentsASDF"
    in createMultipartMixedMessage hdrs "asdf" [p, a]
