-- |
-- Module     : Codec.Compression.SnappyC
-- Copyright  : (c) 2023 Finley McIlwaine
-- License    : BSD-3-Clause (see LICENSE)
--
-- Maintainer : Finley McIlwaine <finley@well-typed.com>
--
-- Haskell bindings to Google's C bindings for their
-- [snappy](https://github.com/google/snappy/) compression library.

module Codec.Compression.SnappyC
  ( -- * Compression
    compress
    -- * Decompression
  , decompress
  ) where

import Codec.Compression.SnappyC.Internal

import Data.ByteString.Internal (ByteString(..))
import Foreign
import System.IO.Unsafe

-- | Compress the input using [snappy](https://github.com/google/snappy/).
--
-- Does not use framing.
compress :: ByteString -> ByteString
compress (BS sfp slen) =
    unsafePerformIO $ do
      let dlen = snappy_max_compressed_length (fromIntegral slen)
      dfp <- mallocForeignPtrBytes (fromIntegral dlen)
      withForeignPtr sfp $ \sptr ->
        withForeignPtr dfp $ \dptr ->
          with dlen $ \dlen_ptr ->
            case
              snappy_compress
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
                error "impossible: we set the buffer size correctly"
              status ->
                error $
                  "impossible: unexpected status from snappy_compress: " ++
                  show status

-- | Decompress the input using [snappy](https://github.com/google/snappy/).
--
-- Returns 'Nothing' if the input is ill-formed.
decompress :: ByteString -> Maybe ByteString
decompress (BS sfp slen) =
    unsafePerformIO $ do
      withForeignPtr sfp $ \sptr ->
        alloca $ \dlen_ptr ->
          case
            snappy_uncompressed_length
              (castPtr sptr)
              (fromIntegral slen)
              dlen_ptr
          of
            0 -> do
              dlen <- fromIntegral <$> peek dlen_ptr
              dfp <- mallocForeignPtrBytes dlen
              withForeignPtr dfp $ \dptr ->
                case
                    snappy_uncompress
                      (castPtr sptr)
                      (fromIntegral slen)
                      (castPtr dptr)
                      dlen_ptr
                of
                  0 ->
                    Just . BS dfp . fromIntegral <$> peek dlen_ptr
                  _ ->
                    return Nothing
            _ ->
              return Nothing
