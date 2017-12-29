import Test.Tasty
import Test.Tasty.QuickCheck

import ContentTransferEncodings as CTE

main :: IO ()
main =
  defaultMain $ testGroup "Tests" [CTE.properties]
