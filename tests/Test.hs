import Test.Tasty
import Test.Tasty.QuickCheck

import ContentTransferEncodings as CTE
import MIME
import Headers
import Generator

main :: IO ()
main =
  defaultMain $ testGroup "Tests"
    [ CTE.properties
    , Headers.unittests
    , Generator.properties
    , MIME.unittests
    ]
