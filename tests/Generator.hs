{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Generator where

import Data.List.NonEmpty (fromList)

import Test.Tasty
import Test.QuickCheck.Instances ()
import Test.Tasty.HUnit ((@=?), testCase)
import Test.Tasty.Golden (goldenVsStringDiff)
import Control.Lens (over, (&), set, at)
import qualified Data.Text.Encoding as T
import Data.Time (Day(..), UTCTime(..), secondsToDiffTime, utc, utcToZonedTime)

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
renderSimpleMailSuccessfully =
    goldenVsStringDiff
        "renders simple mail"
        diffCommand
        "tests/golden/textplain7bit.golden"
        (pure $ renderMessage textPlain7bit)

diffCommand :: FilePath -> FilePath -> [String]
diffCommand ref new =
    [ "diff"
    , "--minimal"
    , "--unified"
    , ref
    , new]

rendersMultiPartSuccessfully :: TestTree
rendersMultiPartSuccessfully =
    goldenVsStringDiff
        "renders simple, multi-part mail"
        diffCommand
        "tests/golden/multipart.golden"
        (pure $ renderMessage multiPartMail)

exampleMailsParseSuccessfully :: TestTree
exampleMailsParseSuccessfully =
    testGroup "example test mails can be round tripped" $
    (\(desc,m) ->
          testCase desc (Right m @=? parse (message mime) (renderMessage m))) <$>
    inputs
  where
    inputs = [("simple mail", textPlain7bit), ("multi part mail", multiPartMail)]

textPlain7bit :: MIMEMessage
textPlain7bit =
    let m = createTextPlainMessage "This is a simple mail."
    in over headers (\(Headers xs) -> Headers $ (CI.mk "Subject", "Hello there") : xs) m

multiPartMail :: MIMEMessage
multiPartMail =
    let from' = Mailbox (Just "Roman Joost") (AddrSpec "foo" (DomainDotAtom $ pure "bar.com"))
        to' = Single $ Mailbox Nothing (AddrSpec "bar" (DomainDotAtom $ pure "bar.com"))
        subject = "Hello there"
        p = createTextPlainMessage "This is a simple mail."
        a = createAttachment
                contentTypeApplicationOctetStream
                (Just "foo.bin")
                "fileContentsASDF"
        nowUTC = UTCTime (ModifiedJulianDay 123) (secondsToDiffTime 123)
        now = utcToZonedTime utc nowUTC
    in createMultipartMixedMessage "asdf" (fromList [p, a])
       & set (headers . at "From") (Just $ renderMailboxes [from'])
       . set (headers . at "To") (Just $ renderAddresses [to'])
       . set (headers . at "Date") (Just $ renderRFC5322Date now)
       . set (headers . at "Subject") (Just $ T.encodeUtf8 subject)
