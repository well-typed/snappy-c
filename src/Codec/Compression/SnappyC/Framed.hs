-- |
-- Module     : Codec.Compression.SnappyC.Framed
-- Copyright  : (c) 2024 Finley McIlwaine
-- License    : BSD-3-Clause (see LICENSE)
--
-- Maintainer : Finley McIlwaine <finley@well-typed.com>
--
-- Frame format snappy compression/decompression.
-- See the framing format description here:
-- <https://github.com/google/snappy/blob/main/framing_format.txt>
--
-- Intended for qualified import:
--
-- > import Codec.Compression.SnappyC.Framed qualified as Snappy

module Codec.Compression.SnappyC.Framed
  ( -- * Compression
    compress

    -- ** Compression with custom parameters
  , EncodeParams(..)
  , FrameSize -- Opaque
  , Threshold(..)
  , compressWithParams
  , defaultFrameSize
  , customFrameSize
  , unFrameSize

    -- * Decompression
  , DecodeFailure(..)
  , decompress
  , decompressLazy

    -- * Low-level incremental API
    -- ** Compression
  , Encoder -- Opaque
  , initializeEncoder
  , finalizeEncoder
  , compressStep

    -- ** Decompression
  , Decoder -- Opaque
  , initializeDecoder
  , finalizeDecoder
  , decompressStep
  , decompressStepLazy
  ) where

import Codec.Compression.SnappyC.Internal.Buffer qualified as Buffer
import Codec.Compression.SnappyC.Internal.FrameFormat
import Codec.Compression.SnappyC.Internal.Util

import Data.ByteString.Internal qualified as Strict (ByteString)
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.Default
import GHC.Stack

-------------------------------------------------------------------------------
-- Compression
-------------------------------------------------------------------------------

-- | Compress the input using [snappy](https://github.com/google/snappy/).
--
-- The output stream is in snappy frame format.
compress :: BS.Lazy.ByteString -> BS.Lazy.ByteString
compress = compressWithParams def

-- | Compress the input using [snappy](https://github.com/google/snappy/) with
-- the given 'EncodeParams'.
--
-- The output stream is in snappy frame format.
compressWithParams :: EncodeParams -> Lazy.ByteString -> Lazy.ByteString
compressWithParams ps =
      BS.Lazy.fromChunks
    . (streamId :)
    . go initialEncoder
    . BS.Lazy.toChunks
  where
    streamId :: Strict.ByteString
    initialEncoder :: Encoder
    (streamId, initialEncoder) = initializeEncoder

    -- Loop invariant: The 'Encoder' has strictly less than 'chunkSize' in it.
    go ::
         Encoder
      -> [Strict.ByteString]
      -> [Strict.ByteString]
    go encoder =
        \case
          [] ->
            finalizeEncoder ps encoder
          (c:cs) ->
            -- The encoded chunks are available to the caller before we process
            -- the rest of the chunks.
            case compressStep ps encoder c of
              (encodedChunks, encoder') ->
                encodedChunks ++ go encoder' cs

-- | Append the data to the 'Encoder' buffer and do as much compression as
-- possible.
--
-- __Postconditions:__
--
-- * The resulting 'Encoder' will not have more data in its buffer than the
--   'chunkSize' in the 'EncodeParams'.
-- * Each encoded frame will hold /exactly/ one `chunkSize` worth of
--   uncompressed data.
compressStep ::
     EncodeParams
  -> Encoder
  -> Strict.ByteString
  -> ([Strict.ByteString], Encoder)
compressStep ps (Encoder b) bs =
    let
      EncodeResult{..} = encodeBuffered ps (Encoder $ b `Buffer.append` bs)
    in
      (encodeResultEncoded, encodeResultEncoder)


-------------------------------------------------------------------------------
-- Decompression
-------------------------------------------------------------------------------

-- | Decompress the input using [snappy](https://github.com/google/snappy/).
--
-- The input stream is expected to be in the official snappy frame format.
-- Evaluates to a 'DecodeFailure' if the input stream is ill-formed.
--
-- __WARNING:__ This function is not as lazy as you might hope. To determine
-- whether the result is a 'DecodeFailure', it must load the entire source
-- 'Lazy.ByteString' into memory during decompression. Use either
-- 'decompressLazy' or the incremental 'decompressStep' instead.
{-# DEPRECATED decompress "Consider using decompressLazy or decompressStep" #-}
decompress :: Lazy.ByteString -> Either DecodeFailure Lazy.ByteString
decompress compressed = do
    decompressedChunks <- go initializeDecoder $ BS.Lazy.toChunks compressed
    return $ BS.Lazy.fromChunks decompressedChunks
  where
    go ::
         Decoder
      -> [Strict.ByteString]
      -> Either DecodeFailure [Strict.ByteString]
    go decoder =
        \case
          [] -> do
            finalizeDecoder decoder
            return []
          (c:cs) -> do
            (decompressed, decompressor') <- decompressStep decoder c
            (decompressed ++) <$> go decompressor' cs

-- | Append the data to the 'Decoder' buffer and do as much decompression as
-- possible.
decompressStep ::
     Decoder
  -> Strict.ByteString
  -> Either DecodeFailure ([Strict.ByteString], Decoder)
decompressStep d@Decoder{..} bs = do
    DecodeResult{..} <-
      decodeBuffered d { decoderBuffer = decoderBuffer `Buffer.append` bs }
    return (decodeResultDecoded, decodeResultDecoder)

-- | Decompress the input using [snappy](https://github.com/google/snappy/).
--
-- The input stream is expected to be in the official snappy frame format.
--
-- __Note:__ The extra laziness of this function (compared to 'decompress')
-- comes at the cost of potential runtime errors during decompression.
decompressLazy :: HasCallStack => Lazy.ByteString -> Lazy.ByteString
decompressLazy =
      BS.Lazy.fromChunks
    . go initializeDecoder
    . BS.Lazy.toChunks
  where
    go ::
         Decoder
      -> [Strict.ByteString]
      -> [Strict.ByteString]
    go decoder =
        \case
          [] ->
            throwLeft (finalizeDecoder decoder) `seq` []
          (c:cs) ->
            let
              (decompressed, decoder') = decompressStepLazy decoder c
            in
              decompressed ++ go decoder' cs

-- | Append the data to the 'Decoder' buffer and do as much decompression as
-- possible.
--
-- This function is slightly lazier than 'decompressStep', as it allows the
-- results of decompression on the given chunk to be streamed in constant
-- memory. The price of the extra laziness: It will throw a runtime exception if
-- any 'DecodeFailure' occurs.
decompressStepLazy ::
     HasCallStack
  => Decoder
  -> Strict.ByteString
  -> ([Strict.ByteString], Decoder)
decompressStepLazy d@Decoder{..} bs =
    let
      DecodeResult{..} =
        decodeBufferedLazy
          d { decoderBuffer = decoderBuffer `Buffer.append` bs }
    in
      ( decodeResultDecoded
      , decodeResultDecoder
      )
