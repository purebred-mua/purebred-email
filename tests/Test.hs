import Test.Tasty

import ContentTransferEncodings as CTE
import EncodedWord
import MIME
import Headers
import Generator
import Parser
import Message

main :: IO ()
main =
  defaultMain $ testGroup "Tests"
    [ CTE.properties
    , EncodedWord.properties
    , Headers.unittests
    , Generator.properties
    , MIME.unittests
    , Parser.tests
    , Message.tests
    ]
