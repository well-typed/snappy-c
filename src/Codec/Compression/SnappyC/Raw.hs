-- |
-- Module     : Codec.Compression.SnappyC.Raw
-- Copyright  : (c) 2024 Finley McIlwaine
-- License    : BSD-3-Clause (see LICENSE)
--
-- Maintainer : Finley McIlwaine <finley@well-typed.com>
--
-- Raw format snappy compression/decompression.
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

-- | Compress the input using [snappy](https://github.com/google/snappy/).
--
-- The result is in snappy raw format, /not/ the framing format.
compress :: ByteString -> ByteString
compress (BS sfp slen) =
    unsafePerformIO $ do
      let dlen = C.snappy_max_compressed_length (fromIntegral slen)
      dfp <- mallocForeignPtrBytes (fromIntegral dlen)
      withForeignPtr sfp $ \sptr ->
        withForeignPtr dfp $ \dptr ->
          with dlen $ \dlen_ptr ->
            case
              C.snappy_compress
                (castPtr sptr)
                (fromIntegral slen)
                (castPtr dptr)
                dlen_ptr
            of
              0 ->
                BS dfp . fromIntegral <$> peek dlen_ptr
              1 ->
                error "impossible: there is no invalid input for compression"
              2 ->
                error "impossible: the buffer size is always set correctly"
              status ->
                error $
                  "impossible: unexpected status from snappy_compress: " ++
                  show status

-- | Decompress the input using [snappy](https://github.com/google/snappy/).
--
-- Returns 'Nothing' if the input is not in snappy raw format or
-- otherwise ill-formed.
decompress :: ByteString -> Maybe ByteString
decompress (BS sfp slen) =
    unsafePerformIO $ do
      withForeignPtr sfp $
        \sptr ->
          alloca $
            \dlen_ptr ->
              case
                C.snappy_uncompressed_length
                  (castPtr sptr)
                  (fromIntegral slen)
                  dlen_ptr
              of
                0 -> do
                  dlen <- fromIntegral <$> peek dlen_ptr
                  dfp <- mallocForeignPtrBytes dlen
                  withForeignPtr dfp $
                    \dptr ->
                      case
                          C.snappy_uncompress
                            (castPtr sptr)
                            (fromIntegral slen)
                            (castPtr dptr)
                            dlen_ptr
                      of
                        0 ->
                          Just . BS dfp . fromIntegral <$> peek dlen_ptr
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

