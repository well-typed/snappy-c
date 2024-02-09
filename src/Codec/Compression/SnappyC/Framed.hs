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
  , decompress'

    -- ** Decompression with custom parameters
  , DecodeParams(..)
  , decompressWithParams
  , decompressWithParams'

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
  , decompressStep'
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
--
-- __Note:__ The extra laziness of this function (compared to `decompress'`)
-- comes at the cost of potential exceptions during decompression.
decompress :: HasCallStack => Lazy.ByteString -> Lazy.ByteString
decompress = decompressWithParams def

-- | Decompress the input using [snappy](https://github.com/google/snappy/) with
-- the given 'DecodeParams'.
--
-- The input stream is expected to be in the official snappy frame format.
--
-- __Note:__ The extra laziness of this function (compared to
-- `decompressWithParams'`) comes at the cost of potential exceptions during
-- decompression.
decompressWithParams ::
     HasCallStack
  => DecodeParams
  -> Lazy.ByteString
  -> Lazy.ByteString
decompressWithParams dps =
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
              (decompressed, decoder') = decompressStep dps decoder c
            in
              decompressed ++ go decoder' cs

-- | Append the data to the 'Decoder' buffer and do as much decompression as
-- possible.
--
-- Throws an exception if any 'DecodeFailure' occurs.
decompressStep ::
     HasCallStack
  => DecodeParams
  -> Decoder
  -> Strict.ByteString
  -> ([Strict.ByteString], Decoder)
decompressStep dps d@Decoder{..} bs =
    let
      DecodeResult{..} =
        throwLeft $
          decodeBuffered
            dps
            d { decoderBuffer = decoderBuffer `Buffer.append` bs }
    in
      ( decodeResultDecoded
      , decodeResultDecoder
      )

-- | Decompress the input using [snappy](https://github.com/google/snappy/).
--
-- The input stream is expected to be in the official snappy frame format.
-- Evaluates to a 'DecodeFailure' if the input stream is ill-formed.
--
-- __WARNING:__ This function is not as lazy as you might hope. To determine
-- whether the result is a 'DecodeFailure', it must load the entire source
-- 'Lazy.ByteString' into memory during decompression. Use either
-- 'decompressWithParams' or the incremental `decompressStep'` instead. If you
-- are truly okay with the extra memory overhead, you may ignore this warning.
{-# DEPRECATED decompress' "Consider using decompress or decompressStep' instead" #-}
decompress' :: Lazy.ByteString -> Either DecodeFailure Lazy.ByteString
decompress' = decompressWithParams' def

-- | Decompress the input using [snappy](https://github.com/google/snappy/) with
-- the given 'DecodeParams'.
--
-- The input stream is expected to be in the official snappy frame format.
-- Evaluates to a 'DecodeFailure' if the input stream is ill-formed.
--
-- __WARNING:__ This function is not as lazy as you might hope. To determine
-- whether the result is a 'DecodeFailure', it must load the entire source
-- 'Lazy.ByteString' into memory during decompression. Use either
-- 'decompressWithParams' or the incremental `decompressStep'` instead. If you
-- are truly okay with the extra memory overhead, you may ignore this warning.
{-# DEPRECATED decompressWithParams' "Consider using decompressWithParams or decompressStep' instead" #-}
decompressWithParams' ::
     DecodeParams
  -> Lazy.ByteString
  -> Either DecodeFailure Lazy.ByteString
decompressWithParams' dps compressed = do
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
            (decompressed, decompressor') <- decompressStep' dps decoder c
            (decompressed ++) <$> go decompressor' cs

-- | Append the data to the 'Decoder' buffer and do as much decompression as
-- possible.
--
-- __Note:__ This function is not as lazy as 'decompressStep', since it must
-- completely decode the given chunk before providing a result.
decompressStep' ::
     DecodeParams
  -> Decoder
  -> Strict.ByteString
  -> Either DecodeFailure ([Strict.ByteString], Decoder)
decompressStep' dps d@Decoder{..} bs = do
    DecodeResult{..} <-
      decodeBuffered dps d { decoderBuffer = decoderBuffer `Buffer.append` bs }
    return (decodeResultDecoded, decodeResultDecoder)
