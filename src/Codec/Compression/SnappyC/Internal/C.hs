{-# LANGUAGE ForeignFunctionInterface #-}

-- | Intended for qualified import:
--
-- > import Codec.Compression.SnappyC.Internal.C qualified as C

module Codec.Compression.SnappyC.Internal.C
  ( -- * Compression
    snappy_compress
  , snappy_max_compressed_length

    -- * Decompression
  , snappy_uncompress
  , snappy_uncompressed_length
  ) where

import Foreign
import Foreign.C

foreign import ccall unsafe "snappy-c.h snappy_compress"
  snappy_compress
    :: CString
    -- ^ Source buffer
    -> CSize
    -- ^ Source buffer size
    -> CString
    -- ^ Target buffer
    -> Ptr CSize
    -- ^ Target buffer size
    -> CInt
    -- ^ Status indicator (0 => Ok, 1 => Invalid input, 2 => Target buffer too small)

foreign import ccall unsafe "snappy-c.h snappy_max_compressed_length"
  snappy_max_compressed_length
    :: CSize
    -- ^ Source buffer size
    -> CSize
    -- ^ Max size after compression

foreign import ccall unsafe "snappy-c.h snappy_uncompress"
  snappy_uncompress
    :: CString
    -- ^ Compressed
    -> CSize
    -- ^ Compressed length
    -> CString
    -- ^ Uncompressed (target) buffer
    -> Ptr CSize
    -- ^ Uncompressed length
    -> CInt
    -- ^ Status indicator (0 => Ok, 1 => Invalid input, 2 => Target buffer too small)

foreign import ccall unsafe "snappy-c.h snappy_uncompressed_length"
  snappy_uncompressed_length
    :: CString
    -- ^ Compressed buffer
    -> CSize
    -- ^ Compressed length
    -> Ptr CSize
    -- ^ Result
    -> CInt
    -- ^ Status indicator (0 => Ok, 1 => Invalid input)
