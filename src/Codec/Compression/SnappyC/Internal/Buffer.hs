-- | Intended for qualified import:
--
-- > import Codec.Compression.SnappyC.Internal.Buffer (Buffer)
-- > import Codec.Compression.SnappyC.Internal.Buffer qualified as Buffer

module Codec.Compression.SnappyC.Internal.Buffer
  ( -- * 'Buffer' type
    Buffer -- Opaque

    -- ** Introduction
  , empty
  , append

    -- ** Elimination
  , toStrict

    -- ** Length etc.
  , length
  , null

    -- ** Splitting
  , splitExactly
  ) where

import Prelude hiding (length, null)

import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString qualified as BS.Strict
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.ByteString.Lazy qualified as BS.Lazy

-- | Intended for efficiency in the case of a bunch of appends followed by a
-- bunch of splits.
data Buffer =
      Forward
        Lazy.ByteString
        !Int
    | Backward
        [Strict.ByteString]
        !Int
  deriving Show

-- | Empty buffer
empty :: Buffer
empty = Backward [] 0

-- | Is the 'Buffer' empty?
--
-- /O(1)/
null :: Buffer -> Bool
null = (== 0) . length

-- | Length
--
-- /O(1)/
length :: Buffer -> Int
length (Forward  _ l) = l
length (Backward _ l) = l

-- | Append data to the end of the 'Buffer'.
--
-- /O(n)/ if the given buffer is forwards, /O(1)/ otherwise.
append :: Buffer -> Strict.ByteString -> Buffer
append b bs =
    let
      !(chunks, l) = backwards b
    in
      Backward (bs : chunks) (l + BS.Strict.length bs)

-- | Get the buffered chunks in backwards order
--
-- /O(n)/ if the given buffer is forwards, /O(1)/ otherwise.
backwards :: Buffer -> ([Strict.ByteString], Int)
backwards (Forward  bs l) = (reverse $ BS.Lazy.toChunks bs, l)
backwards (Backward bs l) = (bs, l)

-- | Get the buffer data as a 'Lazy.ByteString' paired with its length.
--
-- /O(n)/ if the buffer is backward, /O(1)/ otherwise.
toLazy :: Buffer -> (Lazy.ByteString, Int)
toLazy (Forward  bs l) = (bs, l)
toLazy (Backward bs l) = (BS.Lazy.fromChunks $ reverse bs, l)

-- | Create a 'Buffer' from a 'Lazy.ByteString' paired with its length.
--
-- /O(1)/
fromLazy :: (Lazy.ByteString, Int) -> Buffer
fromLazy (bs, l) = Forward bs l

-- | Split off a chunk of exactly @n@ bytes.
--
-- If there aren't enough bytes, return how many bytes we need.
--
-- /O(1)/ if the length is insufficient, /O(n)/ otherwise.
splitExactly :: Int -> Buffer -> Either Int (Strict.ByteString, Buffer)
splitExactly n b
    | length b < n
    = Left (n - length b)
    | otherwise
    = let
        !(bs, bsLength) = toLazy b
        !(tookBs, rest) = BS.Lazy.splitAt (fromIntegral n) bs
      in
        Right
          ( BS.Lazy.toStrict tookBs
          , fromLazy (rest, bsLength - n)
          )

-- | Get the buffer data as a 'Strict.ByteString'. __Only__ call this function if
-- you are __sure__ the data in the buffer is small.
--
-- /O(n)/
--
-- TODO: We are losing size information here that we could use to our advantage
-- in the conversion, but it's a minor inefficiency.
toStrict :: Buffer -> Strict.ByteString
toStrict (Forward  bs _) = BS.Lazy.toStrict bs
toStrict (Backward bs _) = BS.Strict.concat $ reverse bs
