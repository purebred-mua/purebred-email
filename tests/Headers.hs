{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
module Headers where

import Control.Lens
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.String (IsString)
import Data.Word (Word8)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import qualified Data.Attoparsec.Text as AText (parseOnly)
import qualified Data.ByteString.Builder as Builder
import qualified Data.CaseInsensitive as CI
import Data.Either (isLeft)

import Test.Tasty
import Test.Tasty.HUnit (assertBool, (@=?), (@?=), testCase, Assertion)
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances ()

import Data.MIME
import qualified Data.RFC5322.Address.Text as AddressText
  (mailbox, address, renderAddress)

renderField :: (CI.CI B.ByteString, B.ByteString) -> L.ByteString
renderField = Builder.toLazyByteString . buildField

unittests :: TestTree
unittests = testGroup "Headers"
  [ parsesMailboxesSuccessfully
  , parsesTextMailboxesSuccessfully
  , parsesAddressesSuccessfully
  , parsesTextAddressesSuccessfully
  , rendersAddressesToTextSuccessfully
  , testRenderMailboxes
  , rendersFieldsSuccessfully
  , ixAndAt
  , contentTypeTests
  , parameterTests
  , testReply
  , testFromToCcBccOptics
  , testProperty "field rendering round-trip" prop_renderHeadersRoundtrip
  , testProperty "folded fields no longer than 78 chars" prop_foldedUnstructuredLimited
  ]

testFromToCcBccOptics :: TestTree
testFromToCcBccOptics = testGroup "headerFrom/To/Cc/Bcc tests" $
  let
    msg = createTextPlainMessage "hi"
    fromAlice = set (headerFrom defaultCharsets) [Single alice] msg
    fromAliceToBob = set (headerTo defaultCharsets) [Single bob] fromAlice
    fromAliceToCarolAndBob = over (headerTo defaultCharsets) (Single carol :) fromAliceToBob
  in
    [ testCase "From empty" $ view (headerFrom defaultCharsets) msg @?= []
    , testCase "To empty" $ view (headerTo defaultCharsets) msg @?= []
    , testCase "set From alice" $ view (headerFrom defaultCharsets) fromAlice @?= [Single alice]
    , testCase "set To bob" $ view (headerTo defaultCharsets) fromAliceToBob @?= [Single bob]
    , testCase "add To carol" $ view (headerTo defaultCharsets) fromAliceToCarolAndBob @?= [Single carol, Single bob]
    , testCase "removing header" $ has (header "From") (set (headerFrom defaultCharsets) [] fromAlice) @?= False
    ]

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
          , "Subject: Re: Simple Subject\r\n")
        , ( "continuous line"
          , ("Subject", "ThisisalongcontiniousLineWithoutAnyWhiteSpaceandNowSomeGarbageASDFASDFASDFASDF")
          , "Subject: ThisisalongcontiniousLineWithoutAnyWhiteSpaceandNowSomeGarbageASDFASDFASDFASDF\r\n")
        , ( "folding"
          , ( "Received"
            , "from adsl-33-138-215-182-129-129.test.example ([XX.XX.XXX.XXX]) by this.is.another.hostname.example with esmtp (Exim 4.24) id 1Akwaj-00035l-NT for me@test.example; Sun, 25 2004 21:35:09 -0500")
          , "Received: from adsl-33-138-215-182-129-129.test.example ([XX.XX.XXX.XXX])\r\n by this.is.another.hostname.example with esmtp (Exim 4.24) id\r\n 1Akwaj-00035l-NT for me@test.example; Sun, 25 2004 21:35:09 -0500\r\n")
        , ( "folding with long words"
          , ( "X-Test" , "these are short words and more and more and more and all asdfsdf of a suddenALongWordAppears")
          , "X-Test: these are short words and more and more and more and all asdfsdf of\r\n a suddenALongWordAppears\r\n")
        ]

testRenderMailboxes :: TestTree
testRenderMailboxes = testCase "test renderMailboxes" $
  renderMailboxes xs @?= "\"Roman Joost\" <foo@bar.com>, bar@bar.com"
  where
    xs = [ Mailbox (Just "Roman Joost") (AddrSpec "foo" (DomainDotAtom ("bar" :| ["com"])))
         , Mailbox Nothing (AddrSpec "bar" (DomainDotAtom ("bar" :| ["com"])))
         ]

rendersAddressesToTextSuccessfully :: TestTree
rendersAddressesToTextSuccessfully =
  testGroup "renders addresses to text" $
  (\(desc, address, expected) ->
     testCase desc $ expected @=? AddressText.renderAddress address) <$>
  xs
  where
    xs =
      [ ( "single address"
        , Single
            (Mailbox Nothing (AddrSpec "foo" (DomainDotAtom $ pure "bar.com")))
        , "foo@bar.com")
      , ( "group of addresses"
        , Group "Group"
             [ Mailbox
                 (Just "Mr Foo")
                 (AddrSpec "foo" (DomainDotAtom $ pure "bar.com"))
             , Mailbox
                 (Just "Mr Bar")
                 (AddrSpec "bar" (DomainDotAtom $ pure "bar.com"))
             ]
        , "Group:\"Mr Foo\" <foo@bar.com>, \"Mr Bar\" <bar@bar.com>;")
      , ( "group of undisclosed recipients"
        , Group "undisclosed-recipients" []
        , "undisclosed-recipients:;")
      ]

-- | Note some examples are taken from https://tools.ietf.org/html/rfc3696#section-3
mailboxFixtures :: IsString s => [(String, Either String Mailbox -> Assertion, s)]
mailboxFixtures =
    [ ( "address with FQDN"
      , (Right (Mailbox Nothing (AddrSpec "foo" (DomainDotAtom $ "bar" :| ["com"]))) @=?)
      , "foo@bar.com")
    , ( "just with a host name"
      , (Right (Mailbox Nothing (AddrSpec "foo" (DomainDotAtom $ pure "bar"))) @=?)
      , "foo@bar")
    , ( "domain as IPv4"
      , (Right (Mailbox (Just "roman") (AddrSpec "roman" (DomainLiteral "192.168.1.1"))) @=?)
      , "roman <roman@[192.168.1.1]>")
    , ( "domain as IPv6"
      , (Right (Mailbox (Just "roman") (AddrSpec "roman" (DomainLiteral "::1"))) @=?)
      , "roman <roman@[::1]>")
    , ( "without TLD"
      , (Right (Mailbox Nothing (AddrSpec "roman" (DomainDotAtom $ pure "host"))) @=?)
      , "roman@host")
    , ( "with quotes in local-part"
      , (Right (Mailbox Nothing (AddrSpec "roman" (DomainDotAtom $ pure "host"))) @=?)
      , "\"roman\"@host")
    , ( "quoted localpart with @"
      , (Right (Mailbox Nothing (AddrSpec "Abc@def" (DomainDotAtom $ pure "host"))) @=?)
      , "\"Abc\\@def\"@host")
    , ( "whitespace in quoted local-part"
      , (Right (Mailbox Nothing (AddrSpec "Mr Whitespace" (DomainDotAtom $ pure "host"))) @=?)
      , "\"Mr Whitespace\"@host")
    , ( "special chars in local-part"
      , (Right (Mailbox Nothing (AddrSpec "customer/department=shipping" (DomainDotAtom $ pure "host"))) @=?)
      , "<customer/department=shipping@host>")
    , ( "special chars in local-part"
      , (Right (Mailbox Nothing (AddrSpec "!def!xyz%abc" (DomainDotAtom $ pure "host"))) @=?)
      , "!def!xyz%abc@host")
    , ( "garbled address"
      , assertBool "Parse error expected" . isLeft
      , "fasdf@")
    , ( "wrong: comma in front of domain"
      , assertBool "Parse error expected" . isLeft
      , "foo@,bar,com")
    , ( "displayName without quotes but with spaces"
      , (Right (Mailbox (Just "John Doe") (AddrSpec "jdoe" (DomainDotAtom $ "machine" :| ["example"]))) @=?)
      , "John Doe <jdoe@machine.example>"
      )
    ]

parsesMailboxesSuccessfully :: TestTree
parsesMailboxesSuccessfully =
    testGroup "parsing mailboxes" $
    (\(desc,f,input) ->
          testCase desc $ f (AText.parseOnly AddressText.mailbox input)) <$>
    mailboxFixtures

parsesTextMailboxesSuccessfully :: TestTree
parsesTextMailboxesSuccessfully =
    testGroup "parsing mailboxes (text)" $
    (\(desc,f,input) ->
          testCase desc $ f (parseOnly (mailbox defaultCharsets) input)) <$>
    mailboxFixtures

addresses :: IsString s => [(String, Either String Address -> Assertion, s)]
addresses =
    [ ( "single address"
      , (Right (Single (Mailbox Nothing (AddrSpec "foo" (DomainDotAtom $ "bar" :| ["com"])))) @=?)
      , "<foo@bar.com>")
    , ( "group of addresses"
      , (Right
             (Group
                  "Group"
                  [ Mailbox (Just "Mr Foo") (AddrSpec "foo" (DomainDotAtom $ "bar" :| ["com"]))
                  , Mailbox (Just "Mr Bar") (AddrSpec "bar" (DomainDotAtom $ "bar" :| ["com"]))]) @=?)
      , "Group: \"Mr Foo\" <foo@bar.com>, \"Mr Bar\" <bar@bar.com>;")
    , ( "group of undisclosed recipients"
      , (Right (Group "undisclosed-recipients" []) @=?)
      , "undisclosed-recipients:;")
    ]

parsesAddressesSuccessfully :: TestTree
parsesAddressesSuccessfully =
    testGroup "parsing addresses" $
    (\(desc,f,input) -> testCase desc $ f (parseOnly (address defaultCharsets) input))
    <$> addresses

parsesTextAddressesSuccessfully :: TestTree
parsesTextAddressesSuccessfully =
    testGroup "parsing addresses (text)" $
    (\(desc,f,input) -> testCase desc $ f (AText.parseOnly AddressText.address input))
    <$> addresses

-- | Sanity check Ixed and At instances
ixAndAt :: TestTree
ixAndAt = testGroup "Ix and At instances"
  [ testCase "set header" $
      set (at "content-type") (Just "text/plain") empty @?= textPlain
  , testCase "set header (multiple)" $
      set (at "content-type") (Just "text/html") multi
      @?= Headers [("Content-Type", "text/html"), ("Content-Type", "text/plain")]
  , testCase "update header (case differs)" $
      set (at "content-type") (Just "text/html") textPlain @?= textHtml
  , testCase "delete header (one)" $
      sans "content-type" textPlain @?= empty
  , testCase "delete header (one)" $
      sans "content-type" textPlain @?= empty
  , testCase "delete header (multiple)" $
      sans "content-type" multi @?= textPlain
  , testCase "delete header (no match)" $
      sans "subject" textPlain @?= textPlain
  , testCase "ix targets all" $
      toListOf (ix "content-type") multi @?= ["foo/bar", "text/plain"]
  ]


contentTypeTests :: TestTree
contentTypeTests = testGroup "Content-Type header"
  [ testCase "parsing header" $
      view contentType textHtml @?= ctTextHtml
  , testCase "no header yields default" $
      view contentType empty @?= defaultContentType
  , testCase "set when undefined" $
      set contentType ctTextHtml empty @?= textHtml
  , testCase "set when defined (update)" $
      set contentType ctTextHtml textPlain @?= textHtml
  , testCase "update undefined content type" $
      over (contentType . parameterList) (("foo","bar"):) empty @?= defaultFoobar
  , testCase "update defined content type" $
      over (contentType . parameterList) (("foo","bar"):) textHtml @?= textHtmlFoobar
  ]
  where
  ctTextHtml = ContentType "text" "html" (Parameters [])

empty, textPlain, textHtml, multi, defaultFoobar, textHtmlFoobar :: Headers
empty = Headers []
textPlain = Headers [("Content-Type", "text/plain")]
textHtml = Headers [("Content-Type", "text/html")]
multi = Headers [("Content-Type", "foo/bar"), ("Content-Type", "text/plain")]
defaultFoobar = Headers [("Content-Type", "text/plain; foo=bar; charset=us-ascii")]
textHtmlFoobar = Headers [("Content-Type", "text/html; foo=bar")]

parameterTests :: TestTree
parameterTests = testGroup "parameter handling"
  [ testCase "RFC 2231 §3 example" $
      view (contentType . parameter "url")
        (Headers [("Content-Type", "message/external-body; access-type=URL; URL*0=\"ftp://\"; URL*1=\"cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar\"")])
      @?= Just (ParameterValue Nothing Nothing "ftp://cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar")
  , testCase "RFC 2231 §4 example" $
      view (contentType . parameter "title")
        (Headers [("Content-Type", "application/x-stuff; title*=us-ascii'en-us'This%20is%20%2A%2A%2Afun%2A%2A%2A")])
      @?= Just (ParameterValue (Just "us-ascii") (Just "en-us") "This is ***fun***")
  , testCase "RFC 2231 §4.1 example" $
      view (contentType . parameter "title")
        (Headers [("Content-Type", "application/x-stuff; title*0*=us-ascii'en'This%20is%20even%20more%20; title*1*=%2A%2A%2Afun%2A%2A%2A%20; title*2=\"isn't it!\"")])
      @?= Just (ParameterValue (Just "us-ascii") (Just "en") "This is even more ***fun*** isn't it!")
  , testCase "set filename parameter in Content-Disposition" $
      set (contentDisposition . traversed . parameter "filename")
        (Just (ParameterValue Nothing Nothing "foo.pdf"))
        (Headers [("Content-Disposition", "attachment")])
      @?= Headers [("Content-Disposition", "attachment; filename=foo.pdf")]
  , testCase "unset filename parameter in Content-Disposition" $
      set (contentDisposition . traversed . parameter "filename") Nothing
        (Headers [("Content-Disposition", "attachment; foo=bar; filename=foo.pdf")])
      @?= Headers [("Content-Disposition", "attachment; foo=bar")]
  ]

-- Test the 'reply' function
testReply :: TestTree
testReply =
  testGroup "test 'reply' function" $
    [ testCase "Message-ID -> In-Reply-To" $
        view headerInReplyTo rep1 @?= [msg1ID]
    , testCase "Message-Id -> References" $
        view headerReferences rep1 @?= [msg1ID]
    , testCase "References + Message-Id -> References" $
        view headerReferences rep2 @?= [msg1ID, rep1ID]
    , testCase "In-Reply-To (no References) + Message-Id -> References" $
        view headerReferences rep_noRef_IRT @?= [msg1ID, rep1ID]
    , testCase "multi In-Reply-To (no Ref) + Message-Id -> Ref = [msgid]" $
        view headerReferences rep_noRef_2IRT @?= [rep1ID]

    , testCase "prepends 'Re: ' to Subject" $
        view (headerSubject defaultCharsets) rep1
          @?= Just "Re: Hello, world!"
    , testCase "doesn't prepend 'Re: ' if already a prefix" $
        view (headerSubject defaultCharsets) rep2
          @?= Just "Re: Hello, world!"

    , testCase "GroupReply (remove self)" $ do
        view (headerFrom defaultCharsets) msg1
          @=? view (headerTo defaultCharsets) rep1
        view (headerTo defaultCharsets) rep1
          @?= [Single alice]
        view (headerCC defaultCharsets) rep1
          @?= [Single carol, Single frank]

    , testCase "GroupReply (ignore self)" $ do
        view (headerFrom defaultCharsets) msg1
          @=? view (headerTo defaultCharsets) rep1'
        view (headerTo defaultCharsets) rep1'
          @?= [Single alice]
        view (headerCC defaultCharsets) rep1'
          @?= [Single bob, Single carol, Single frank]

    , testCase "SingleReply" $ do
        view (headerFrom defaultCharsets) rep1
          @=? view (headerTo defaultCharsets) rep2
        view (headerCC defaultCharsets) rep2 @=? []

    , testCase "Reply-To -> To" $
        view (headerTo defaultCharsets) rep_ReplyTo @?= [Single frank]
    ]
  where
    mkSettings = ReplySettings
      ReplyToSender
      ReplyFromMatchingMailbox
      ReplyFromRewriteOn
      SelfInRecipientsRemove
    bobSettings   = mkSettings (pure bob) & set replyMode ReplyToGroup
    carolSettings = mkSettings (pure carol)

    extraMsgId = (\(Right a) -> a) $ parseOnly parseMessageID "<extra@host>"

    msg1ID = (\(Right a) -> a) $ parseOnly parseMessageID "<msg1@host>"
    msg1 = createTextPlainMessage "hello, world!"
      & set headerMessageID (Just msg1ID)
      . set (headerSubject defaultCharsets) (Just "Hello, world!")
      . set (headerFrom defaultCharsets) [Single alice]
      . set (headerTo defaultCharsets) [Single bob, Single carol]
      . set (headerCC defaultCharsets) [Single frank]

    rep1ID = (\(Right a) -> a) $ parseOnly parseMessageID "<rep1@host>"
    rep1 = reply defaultCharsets bobSettings msg1
      & set headerMessageID (Just rep1ID)
    rep1' =  -- same as rep1, but with SelfInRecipientsIgnore
      let bobSettings' =
            bobSettings & set selfInRecipientsMode SelfInRecipientsIgnore
      in reply defaultCharsets bobSettings' msg1
          & set headerMessageID (Just rep1ID)

    rep2ID = (\(Right a) -> a) $ parseOnly parseMessageID "<rep2@host>"
    rep2 = reply defaultCharsets carolSettings rep1
      & set headerMessageID (Just rep2ID)

    -- reply to a message with no References + single-valued In-Reply_To
    rep_noRef_IRT =
      reply defaultCharsets carolSettings (set headerReferences [] rep1)

    -- reply to a message with no References + multi-valued In-Reply_To
    rep_noRef_2IRT =
      reply defaultCharsets carolSettings $
        rep1
          & set headerReferences []
          & over headerInReplyTo (extraMsgId:)

    -- reply to a message with Reply-To header set
    rep_ReplyTo =
      reply defaultCharsets carolSettings
        (set (headerReplyTo defaultCharsets) [Single frank] msg1)


-- | Generate headers
genFieldItem :: Gen B.ByteString
genFieldItem = resize 55 (B.pack <$> listOf1 (suchThat arbitrary isFtext))

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

prop_renderHeadersRoundtrip :: Property
prop_renderHeadersRoundtrip = forAll genField $ \kv ->
  parse field (renderField kv) == Right kv

prop_foldedUnstructuredLimited :: Property
prop_foldedUnstructuredLimited = forAll genField $ \kv ->
  all ((<= 78) . L.length) (crlfLines $ renderField kv)

crlfLines :: L.ByteString -> [L.ByteString]
crlfLines = go ""
  where
  go acc s =
    let (h,t) = L8.span (/= '\r') s
    in
      case L.take 2 t of
        ""      -> [acc <> h]
        "\r\n"  -> acc <> h : go "" (L.drop 2 t)
        _       -> go (acc <> h <> L.take 1 t) (L.drop 1 t)

alice, bob, carol, frank :: Mailbox
alice = "alice@example.com"
bob = "bob@example.com"
carol = "carol@example.com"
frank = "frank@example.com"
