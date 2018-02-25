{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
module Headers where

import qualified Data.ByteString.Char8 as BC
import Data.Attoparsec.ByteString.Char8 (parseOnly)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?), testCase, Assertion)

import Data.MIME


unittests :: TestTree
unittests =
    testGroup
        "address parsing tests"
        [ parsesMailboxesSuccessfully
        , parsesAddressesSuccessfully
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
