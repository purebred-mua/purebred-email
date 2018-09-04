import Test.Tasty
import Test.Tasty.QuickCheck

import ContentTransferEncodings as CTE
import Headers
import MIME

main :: IO ()
main =
  defaultMain $ testGroup "Tests"
    [ CTE.properties
    , Headers.unittests
    , MIME.unittests
    ]
