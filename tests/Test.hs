import Test.Tasty
import Test.Tasty.QuickCheck

import ContentTransferEncodings as CTE
import Headers as Headers

main :: IO ()
main =
  defaultMain $ testGroup "Tests" [CTE.properties, Headers.unittests]
