{-

Parse all mail files given (linewise by filename) on stdin.
Print filenames that had parse failures, along with an attempted
diagnosis of the problem.

-}

import Data.Foldable (traverse_)
import Data.Semigroup ((<>))
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as L
import Data.MIME


main :: IO ()
main = do
  files <- lines <$> getContents
  fileFilter <- processOpts . pairwise <$> getArgs
  traverse_ parseMail (fileFilter files)

parseMail :: FilePath -> IO ()
parseMail filename = do
  msgData <- L.readFile filename
  case parse (message mime) msgData of
    Left e -> putStrLn (filename <> " (" <> e <> ") " <> analyse msgData)
    Right _ -> pure ()

analyse :: L.ByteString -> String
analyse s = fst $ head $ filter snd $ fmap (fmap ($ s)) tests
  where
  tests =
    [ ("non-ascii chars", L.any (> 127))
    , ("unknown", const True)
    ]

pairwise :: [a] -> [(a, a)]
pairwise (k:v:t) = (k, v) : pairwise t
pairwise (_:_) = error "uneven number of args"
pairwise [] = []

-- Process options, returning a function for how to filter
-- the list of files
--
-- Recognised options:
--
-- ("-l", N) -> limit to first N files in inputs
--
processOpts :: [(String, String)] -> ([String] -> [String])
processOpts [] = id
processOpts (("-l", s):t) = take (read s) . processOpts t
processOpts (opt:_) = error ("unrecognised option: " <> show opt)
