-- | Intended for unqualified import:
--
-- > import Codec.Compression.SnappyC.Internal.Util

module Codec.Compression.SnappyC.Internal.Util
  ( -- * Exceptions
    throwLeft
  ) where

import Control.Exception

-- | If a 'Left' is given, throw the contained value as an exception.
throwLeft :: Exception e => Either e a -> a
throwLeft (Left err) = throw err
throwLeft (Right x)  = x
