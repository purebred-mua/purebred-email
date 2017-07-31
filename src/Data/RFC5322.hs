module Data.RFC5322
  where

{- |

Email messages.  Deals specifically with RFC 5322, which is stricter
than RFC 822 or RFC 2822.  If you have to deal with messages that
comply with the older specifications but not RFC 5322, preprocess
the input and massage it to be RFC 5322 compliant.

-}

import Control.Applicative

import qualified Data.Map as M
import qualified Data.ByteString as B

type Headers = M.Map B.ByteString B.ByteString

data RFC5322 a = RFC5322 Headers a

type Body = B.ByteString
