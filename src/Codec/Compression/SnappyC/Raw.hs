-- |
-- Module     : Codec.Compression.SnappyC.Raw
-- Copyright  : (c) 2024 Finley McIlwaine
-- License    : BSD-3-Clause (see LICENSE)
--
-- Maintainer : Finley McIlwaine <finley@well-typed.com>
--
-- Raw format Snappy compression/decompression.
--
-- > import Codec.Compression.SnappyC.Raw qualified as Snappy

module Codec.Compression.SnappyC.Raw
  ( -- * Compression
    compress
    -- * Decompression
  , decompress
  ) where

import Codec.Compression.SnappyC.Internal.C qualified as C

import Data.ByteString.Internal (ByteString(..))
import Foreign
import System.IO.Unsafe
import Data.ByteString.Unsafe

-- | Compress the input using [Snappy](https://github.com/google/snappy/).
--
-- The result is in Snappy raw format, /not/ the framing format.
compress :: ByteString -> ByteString
compress bs = unsafePerformIO $ do
    unsafeUseAsCStringLen bs $ \(sptr, slen) -> do
      let dlen = C.snappy_max_compressed_length (fromIntegral slen)
      dptr <- mallocBytes (fromIntegral dlen)
      with dlen $ \dlen_ptr ->
        case
          C.snappy_compress
            (castPtr sptr)
            (fromIntegral slen)
            (castPtr dptr)
            dlen_ptr
        of
          0 -> do
            len <- fromIntegral <$> peek dlen_ptr
            unsafePackMallocCStringLen (dptr, len)
          1 ->
            error "impossible: there is no invalid input for compression"
          2 ->
            error "impossible: the buffer size is always set correctly"
          status ->
            error $
              "impossible: unexpected status from snappy_compress: " ++
              show status

-- | Decompress the input using [Snappy](https://github.com/google/snappy/).
--
-- Returns 'Nothing' if the input is not in Snappy raw format or
-- otherwise ill-formed.
decompress :: ByteString -> Maybe ByteString
decompress bs = unsafePerformIO $ do
    unsafeUseAsCStringLen bs $ \(sptr, slen) ->
      alloca $ \dlen_ptr ->
        case
          C.snappy_uncompressed_length
            (castPtr sptr)
            (fromIntegral slen)
            dlen_ptr
        of
          0 -> do
            dlen <- fromIntegral <$> peek dlen_ptr
            dptr <- mallocBytes dlen
            case
                C.snappy_uncompress
                  (castPtr sptr)
                  (fromIntegral slen)
                  (castPtr dptr)
                  dlen_ptr
             of
              0 -> do
                len <- fromIntegral <$> peek dlen_ptr
                Just <$> unsafePackMallocCStringLen (dptr, len)
              1 ->
                -- Invalid input. Successful result from
                -- snappy_uncompressed_length does *not* mean the
                -- input is completely valid
                return Nothing
              status ->
                error $
                  "impossible: decompression failed with status " ++
                  show status
          1 ->
            return Nothing
          status ->
            error $
              "impossible: snappy_uncompressed_length failed with " ++
              "status" ++ show status

