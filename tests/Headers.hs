{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
module Headers where

import Data.Word (Word8)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString.Char8 (parseOnly)

import Data.Semigroup ((<>))
import qualified Data.CaseInsensitive as CI

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty.HUnit ((@=?), testCase, Assertion)

import Data.MIME


unittests :: TestTree
unittests =
    testGroup
        "message field tests"
        [ testGroup
              "address parsing tests"
              [parsesMailboxesSuccessfully, parsesAddressesSuccessfully, rendersFieldsSuccessfully]
        , testProperty "field rendering round-trip" prop_renderHeadersRoundtrip
        , testProperty "folded fields no longer than 78 chars" prop_foldedUnstructuredLimited]

rendersFieldsSuccessfully :: TestTree
rendersFieldsSuccessfully =
    testGroup "correct folding for unstructured" $
    (\(description,h,expected) ->
          testCase description (expected @=? renderField h)) <$>
    inputs
  where
    inputs =
        [ ( "no folding"
          , ("Subject", "Re: Simple Subject")
          , "Subject: Re: Simple Subject\n")
        , ( "continuous line"
          , ("Subject", "ThisisalongcontiniousLineWithoutAnyWhiteSpaceandNowSomeGarbageASDFASDFASDFASDF")
          , "Subject: \r\n ThisisalongcontiniousLineWithoutAnyWhiteSpaceandNowSomeGarbageASDFASDFASDFASDF\n")
        , ( "folding"
          , ( "Received"
            , "from adsl-33-138-215-182-129-129.test.example ([XX.XX.XXX.XXX]) by this.is.another.hostname.example with esmtp (Exim 4.24) id 1Akwaj-00035l-NT for me@test.example; Sun, 25 2004 21:35:09 -0500")
          , "Received: from adsl-33-138-215-182-129-129.test.example ([XX.XX.XXX.XXX])\r\n by this.is.another.hostname.example with esmtp (Exim 4.24) id\r\n 1Akwaj-00035l-NT for me@test.example; Sun, 25 2004 21:35:09 -0500\n")
        , ( "folding with long words"
          , ( "X-Test" , "these are short words and more and more and more and all asdfsdf of a suddenALongWordAppears")
          , "X-Test: these are short words and more and more and more and all asdfsdf of\r\n a suddenALongWordAppears\n")
        ]

-- | Note some examples are taken from https://tools.ietf.org/html/rfc3696#section-3
mailboxes :: [(String, Either String Mailbox -> Assertion, BC.ByteString)]
mailboxes =
    [ ( "address with FQDN"
      , (Right (Mailbox Nothing (AddrSpec "foo" (DomainDotAtom $ pure "bar.com"))) @=?)
      , BC.pack "foo@bar.com")
    , ( "just with a host name"
      , (Right (Mailbox Nothing (AddrSpec "foo" (DomainDotAtom $ pure "bar"))) @=?)
      , BC.pack "foo@bar")
    , ( "domain as IPv4"
      , (Right (Mailbox (Just "roman") (AddrSpec "roman" (DomainLiteral "192.168.1.1"))) @=?)
      , BC.pack "roman <roman@[192.168.1.1]>")
    , ( "domain as IPv6"
      , (Right (Mailbox (Just "roman") (AddrSpec "roman" (DomainLiteral "::1"))) @=?)
      , BC.pack "roman <roman@[::1]>")
    , ( "without TLD"
      , (Right (Mailbox Nothing (AddrSpec "roman" (DomainDotAtom $ pure "host"))) @=?)
      , BC.pack "roman@host")
    , ( "with quotes in local-part"
      , (Right (Mailbox Nothing (AddrSpec "roman" (DomainDotAtom $ pure "host"))) @=?)
      , BC.pack "\"roman\"@host")
    , ( "quoted localpart with @"
      , (Right (Mailbox Nothing (AddrSpec "Abc@def" (DomainDotAtom $ pure "host"))) @=?)
      , BC.pack "\"Abc\\@def\"@host")
    , ( "whitespace in quoted local-part"
      , (Right (Mailbox Nothing (AddrSpec "Mr Whitespace" (DomainDotAtom $ pure "host"))) @=?)
      , BC.pack "\"Mr Whitespace\"@host")
    , ( "special chars in local-part"
      , (Right (Mailbox Nothing (AddrSpec "customer/department=shipping" (DomainDotAtom $ pure "host"))) @=?)
      , BC.pack "<customer/department=shipping@host>")
    , ( "special chars in local-part"
      , (Right (Mailbox Nothing (AddrSpec "!def!xyz%abc" (DomainDotAtom $ pure "host"))) @=?)
      , BC.pack "!def!xyz%abc@host")
    , ( "garbled address"
      , (Left "[: not enough input" @=?)
      , BC.pack "fasdf@")
    , ( "wrong: comma in front of domain"
      , (Left "[: Failed reading: satisfy" @=?)
      , BC.pack "foo@,bar,com")
    ]

parsesMailboxesSuccessfully :: TestTree
parsesMailboxesSuccessfully =
    testGroup "parsing mailboxes" $
    (\(desc,f,input) -> testCase desc $ f (parseOnly mailbox input))
    <$> mailboxes

addresses :: [(String, Either String Address -> Assertion, BC.ByteString)]
addresses =
    [ ( "single address"
      , (Right (Single (Mailbox Nothing (AddrSpec "foo" (DomainDotAtom $ pure "bar.com")))) @=?)
      , BC.pack "<foo@bar.com>")
    , ( "group of addresses"
      , (Right
             (Group
                  "Group"
                  [ Mailbox (Just "Mr Foo") (AddrSpec "foo" (DomainDotAtom $ pure "bar.com"))
                  , Mailbox (Just "Mr Bar") (AddrSpec "bar" (DomainDotAtom $ pure "bar.com"))]) @=?)
      , BC.pack "Group: \"Mr Foo\" <foo@bar.com>, \"Mr Bar\" <bar@bar.com>;")
    , ( "group of undisclosed recipients"
      , (Right (Group "undisclosed-recipients" []) @=?)
      , BC.pack "undisclosed-recipients:;")
    ]

parsesAddressesSuccessfully :: TestTree
parsesAddressesSuccessfully =
    testGroup "parsing mailboxes" $
    (\(desc,f,input) -> testCase desc $ f (parseOnly address input))
    <$> addresses

-- | Generate headers
genFieldItem :: Gen B.ByteString
genFieldItem = resize 55 $ listOf1 (suchThat arbitrary isFtext) >>= pure . B.pack

isFtext :: Word8 -> Bool
isFtext c = (c >= 33 && c <= 57) || (c >= 59 && c <= 126)

-- * generate a growing list of words
-- * join all words by a whitespace
--
genFieldBody :: Gen B.ByteString
genFieldBody = do
  nonWspStart <- suchThat arbitrary vchar
  t <- listOf1 $ resize 30 $ listOf1 (suchThat arbitrary vchar)
  pure (nonWspStart `B.cons` foldl (\acc x -> acc <> " " <> B.pack x) B.empty t) -- urks that's going to be costly

vchar :: Word8 -> Bool
vchar c = c >= 33 && c <= 126

genField :: Gen (CI.CI B.ByteString, B.ByteString)
genField = (,) <$> (CI.mk <$> genFieldItem) <*> genFieldBody

newtype MailHeaders = MailHeaders { unHeader :: (CI.CI B.ByteString, B.ByteString)}
  deriving (Show)

instance Arbitrary MailHeaders where
    arbitrary = MailHeaders <$> genField

prop_renderHeadersRoundtrip :: MailHeaders -> Bool
prop_renderHeadersRoundtrip h = parse field (renderField (unHeader h)) == Right (unHeader h)

prop_foldedUnstructuredLimited :: MailHeaders -> Bool
prop_foldedUnstructuredLimited h = let xs = BC.lines $ renderField (unHeader h)
                                   in all (== True) ((\x -> B.length x <= 78) <$> xs)
