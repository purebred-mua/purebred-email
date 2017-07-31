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

import qualified Data.Map as M
import qualified Data.ByteString as B

type Headers = M.Map B.ByteString B.ByteString

data Part
  = Part Headers Body
  | Multipart [Part]

data Body = Body B.ByteString | BodyExtents Int Int
