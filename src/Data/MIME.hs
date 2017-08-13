{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Data.MIME
  where

{- |

MIME messages (RFC 2045, RFC 2046 and friends).

There are different approaches to parsing the MIME content bodies,
to account for different use cases.

- Parse a body into a @ByteString@ (transfer encoding ignored)

- Parse a body into start offset and length.  The content is
  not included in the parsed data.  This mode is suitable e.g.
  for attachments, where there is no point reading the data into
  the program but you need enough information to read the body
  again at a later time.

The parser is configured with a function that tells it which body
type to use for a given part.  Multipart messages are handled
specially, as part of the 'MIME' data type.

-}

import Control.Applicative
import Data.Word (Word8)

import Control.Lens
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B

import Data.RFC5322
import Data.RFC5322.Internal

data Part
  = Part Headers Body
  | Multipart [Part]


data ContentType = ContentType
  (CI B.ByteString) -- type
  (CI B.ByteString) -- subtype
  [(CI B.ByteString, B.ByteString)]  -- parameters
  deriving (Show)

ctType :: Lens' ContentType (CI B.ByteString)
ctType f (ContentType a b c) = fmap (\a' -> ContentType a' b c) (f a)

ctParameters :: Lens' ContentType [(CI B.ByteString, B.ByteString)]
ctParameters f (ContentType a b c) = fmap (\c' -> ContentType a b c') (f c)

-- | https:\/\/tools.ietf.org\/html\/rfc2045#section-5.1
contentType :: Parser ContentType
contentType = ContentType
  <$> ci token
  <*  word8 47 {-/-} <*> ci token
  <*> many (word8 59 {-;-} *> skipWhile (== 32 {-SP-}) *> parameter)
  where
    parameter = (,) <$> ci token <* word8 61 {-=-} <*> value
    value = token <|> quotedString
    token = takeWhile1 (\c -> c >= 33 && c <= 126 && notInClass "()<>@,;:\\\"/[]?=" c)


-- | Given a parser, construct a 'Fold'
parsed :: Parser a -> Fold B.ByteString a
parsed p = to (parseOnly p) . folded


mime :: Parser Part
mime = message >>= \(RFC5322 hdrs (Just body)) -> do -- FIXME irref pat
  case preview (header "content-type" . parsed contentType) hdrs of
    Just ct | view ctType ct == "multipart" ->
      case preview (ctParameters . header "boundary") ct of
        Nothing -> pure $ Part hdrs body
        Just boundary -> multipart boundary
    _ -> pure $ Part hdrs body

multipart
  :: B.ByteString  -- ^ boundary, sans leading "--"
  -> Parser Part
multipart = undefined
