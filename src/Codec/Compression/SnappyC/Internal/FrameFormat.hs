-- | Intended for unqualified import:
--
-- > import module Codec.Compression.SnappyC.Internal.FrameFormat

module Codec.Compression.SnappyC.Internal.FrameFormat
  ( -- * The 'Frame' type
    Frame(..)
  , FrameIdentifier(..)

    -- ** Encoding
  , Encoder(..)
  , EncodeParams(..)
  , FrameSize -- Opaque
  , Threshold(..)
  , EncodeState(..)
  , EncodeResult(..)
  , initializeEncoder
  , finalizeEncoder
  , encodeBuffered
  , customFrameSize
  , unFrameSize
  , defaultFrameSize

    -- ** Decoding
  , Decoder(..)
  , DecodeState(..)
  , DecodeResult(..)
  , DecodeFailure(..)
  , initializeDecoder
  , finalizeDecoder
  , decodeBuffered
  , decodeBufferedLazy
  ) where

import Codec.Compression.SnappyC.Internal.Buffer (Buffer)
import Codec.Compression.SnappyC.Internal.Buffer qualified as Buffer
import Codec.Compression.SnappyC.Internal.Checksum (Checksum)
import Codec.Compression.SnappyC.Internal.Checksum qualified as Checksum
import Codec.Compression.SnappyC.Internal.Util
import Codec.Compression.SnappyC.Raw qualified as Raw

import Control.Exception
import Control.Monad.Error.Class
import Data.Bits
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString qualified as BS.Strict
import Data.Default
import Data.Word
import Text.Printf

-- | Snappy frames consist of a header and a payload.
data Frame =
      Frame
        { frameHeader :: !FrameHeader
        , framePayload :: !Strict.ByteString
        }
  deriving Show

-- | A frame's header contains an identifier corresponding to the frame type and
-- the size of the payload.
data FrameHeader =
      FrameHeader
        { frameHeaderIdentifier :: !FrameIdentifier
        , frameHeaderPayloadSize :: !Int
        }
  deriving (Show, Eq)

-- | Snappy frame identifiers.
data FrameIdentifier =
      StreamId
    | Compressed
    | Uncompressed
    | Padding
    | ReservedUnskippable Word8
    | ReservedSkippable Word8
  deriving (Show, Eq)

-- | The one byte value corresponding to an identifier.
encodeFrameIdentifier :: FrameIdentifier -> Word8
encodeFrameIdentifier StreamId                  = 0xff
encodeFrameIdentifier Compressed                = 0x00
encodeFrameIdentifier Uncompressed              = 0x01
encodeFrameIdentifier Padding                   = 0xfd
encodeFrameIdentifier (ReservedUnskippable fid) = fid
encodeFrameIdentifier (ReservedSkippable fid)   = fid

-- | Encode an 'Int' as a three byte little-endian value.
encodeFrameHeader :: FrameHeader -> Strict.ByteString
encodeFrameHeader (FrameHeader ident size)=
    BS.Strict.pack
      [ encodeFrameIdentifier ident
      , fromIntegral   (w32Size .&. 0x000000ff)
      , fromIntegral $ (w32Size .&. 0x0000ff00) `shiftR` 8
      , fromIntegral $ (w32Size .&. 0x00ff0000) `shiftR` 16
      ]
  where
    w32Size :: Word32
    w32Size = fromIntegral size

-------------------------------------------------------------------------------
-- Encoding snappy frames
-------------------------------------------------------------------------------

-- | Buffers uncompressed data for compression.
newtype Encoder = Encoder { encoderBuffer :: Buffer }
  deriving Show

-- | Determines how much data is put in each snappy frame and whether it is
-- compressed.
data EncodeParams =
      EncodeParams
        { -- | Exact amount of uncompressed data included in a single frame.
          frameSize :: !FrameSize

          -- | Compression threshold.
        , threshold :: !Threshold
        }
  deriving (Show, Eq)

instance Default EncodeParams where
  def :: EncodeParams
  def = EncodeParams def def

-- | Number of bytes of uncompressed data.
newtype FrameSize = FrameSize Int
  deriving (Show, Eq)

instance Default FrameSize where
    def :: FrameSize
    def = defaultFrameSize

-- | The default frame size is 65536 bytes, which is the maximum allowed by the
-- [snappy framing format
-- description](https://github.com/google/snappy/blob/main/framing_format.txt).
defaultFrameSize :: FrameSize
defaultFrameSize = FrameSize snappySpecMaxChunkBytes

-- | See section 4.2 of the [snappy framing format
-- description](https://github.com/google/snappy/blob/main/framing_format.txt).
snappySpecMaxChunkBytes :: Int
snappySpecMaxChunkBytes = 65536

customFrameSize :: Int -> FrameSize
customFrameSize n
    | n >= 1 && n <= snappySpecMaxChunkBytes
    = FrameSize n
    | otherwise
    = error "customFrameSize: invalid frame size"

-- | Unwrap a 'FrameSize'
unFrameSize :: FrameSize -> Int
unFrameSize (FrameSize n) = n

-- | Compression threshold, with explicit 'AlwaysCompress' and 'NeverCompress'
-- settings.
data Threshold =
      -- | Compress everything
      AlwaysCompress

      -- | Compress nothing
    | NeverCompress

      -- | Uncompressed size divided by compressed size.
      --
      -- Only produce compressed frames if the compression ratio for the data is
      -- equal to or above this threshold.
      --
      -- A higher threshold may result in less frames holding compressed data,
      -- and thus faster decompression/decoding.
      --
      -- [According to
      -- Google](https://github.com/google/snappy?tab=readme-ov-file#performance),
      -- the typical highest compression ratio that snappy achieves is about 4,
      -- so a 'Ratio' of > 4.0 should be similar to 'NeverCompress', while a
      -- 'Ratio' of < 7/8 should be similar to 'AlwaysCompress'.
    | Ratio !Double
  deriving (Show, Eq)

instance Default Threshold where
  def :: Threshold
  def = defaultThreshold

-- | The default threshold is a ratio of 8:7, which was taken from the
-- [golang/snappy
-- implementation](https://github.com/golang/snappy/blob/43d5d4cd4e0e3390b0b645d5c3ef1187642403d8/encode.go#L231).
defaultThreshold :: Threshold
defaultThreshold = Ratio (1 / 0.875)

-- | Determines how much uncompressed data is stored in each resulting frame.
newtype EncodeState =
      EncodeState { encodeStateMaxChunkBytes :: Int }
  deriving Show

-- | A pair of frame-encoded chunks and an updated 'Encoder'.
data EncodeResult =
      EncodeResult
        { encodeResultEncoded :: [Strict.ByteString]
        , encodeResultEncoder :: !Encoder
        }
  deriving Show

-- | Initialize an 'Encoder' with the given maximum number of bytes of
-- uncompressed data to include in frames resulting from the 'Encoder'. If the
-- given number of bytes is not in the inclusive range [1 .. 65536], 65536 is
-- used.
--
-- The 'Strict.ByteString' holds the snappy stream identifier frame that must be
-- included at the start of every snappy frame encoded stream.
initializeEncoder :: (Strict.ByteString, Encoder)
initializeEncoder =
    ( "\xff\x06\x00\00sNaPpY"
    , Encoder
        { encoderBuffer = Buffer.empty
        }
    )

-- | Call to indicate no more input and flush the remaining data in the
-- 'Encoder' into a new frame.
--
-- If there is no more data in the 'Encoder', an empty list is returned.
--
-- * __Precondition:__ The buffer does not hold more data than the 'frameSize'
--   in the 'EncodeParams'. Use the postcondition of 'encodeBuffered' to ensure
--   this.
finalizeEncoder :: EncodeParams -> Encoder -> [Strict.ByteString]
finalizeEncoder ep (Encoder b)
    | Buffer.null b
    = []
    | otherwise
    = encodeChunk ep (Buffer.toStrict b)

-- | Fill and compress/encode as many frames as possible with the data in the
-- 'Encoder'.
--
-- /O(1)/ if there are not enough bytes in the buffer to fill a frame.
--
-- * __Postcondition:__ The resulting buffer never holds more than the
--   'frameSize' given in the 'EncodeParams'.
encodeBuffered :: EncodeParams -> Encoder -> EncodeResult
encodeBuffered ep@EncodeParams{..} = \(Encoder b) ->
    go [] b
  where
    go :: [Strict.ByteString] -> Buffer -> EncodeResult
    go acc b =
        case Buffer.splitExactly (unFrameSize frameSize) b of
          Right (chunk, b') ->
            go (reverse (encodeChunk ep chunk) ++ acc) b'
          Left _ ->
            EncodeResult
              (reverse acc)
              (Encoder b)

-- | Encode the input as a potentially compressed snappy frame.
--
-- This function takes a 'Strict.ByteString' because it must pass all of the
-- data to a C function which expects the data to sit in a single buffer.
encodeChunk ::
     EncodeParams
  -> Strict.ByteString
  -> [Strict.ByteString]
encodeChunk EncodeParams{..} uncompressed =
    [ encodeFrameHeader
        (FrameHeader frameId (BS.Strict.length payloadData + 4))
    , Checksum.encode maskedChecksum
    , payloadData
    ]
  where
    maskedChecksum :: Checksum
    maskedChecksum = Checksum.calculate uncompressed

    compressed :: Strict.ByteString
    compressed = Raw.compress uncompressed

    compressionRatio :: Double
    compressionRatio =
        (/) @Double
          (fromIntegral $ BS.Strict.length uncompressed)
          (fromIntegral $ BS.Strict.length compressed)

    (frameId, payloadData) =
        if doCompress then
          (Compressed, compressed)
        else
          (Uncompressed, uncompressed)

    doCompress =
        case threshold of
          AlwaysCompress -> True
          NeverCompress  -> False
          Ratio ratio    -> compressionRatio >= ratio

-------------------------------------------------------------------------------
-- Decoding snappy frames
-------------------------------------------------------------------------------

-- | Buffers compressed data for decompression and holds some useful
-- decompression state.
data Decoder =
      Decoder
        { -- | Accumulated snappy framed data.
          --
          -- * __Invariant:__ This buffer never holds a fully decodable snappy
          --   frame.
          decoderBuffer :: !Buffer

          -- | Tracks partial information about the buffer, e.g. whether we have
          -- decoded a header and how many bytes we need to fully decode a frame.
        , decoderState :: !DecodeState
        }
  deriving Show

-- | Have we decoded a header for the current frame yet?
--
-- If so, what was that header?
data DecodeState =
      Initial
    | KnownHeader !FrameHeader
  deriving (Show, Eq)

-- | Pair of decompressed data chunks and an updated 'Decoder'.
data DecodeResult =
      DecodeResult
        { decodeResultDecoded :: [Strict.ByteString]
        , decodeResultDecoder :: !Decoder
        }
  deriving Show

-- | Possible failure modes for decompression.
data DecodeFailure =
      DecompressionError Strict.ByteString
    | ReservedUnskippableFrameId Word8
    | BadStreamId Strict.ByteString
    | BadChecksum
        Strict.ByteString -- ^ Data
        Checksum -- ^ Received
        Checksum -- ^ Computed
    | NotDone
  deriving Show
  deriving anyclass Exception

-- | The empty 'Decoder', in an initial state.
initializeDecoder :: Decoder
initializeDecoder =
    Decoder
      { decoderBuffer = Buffer.empty
      , decoderState = Initial
      }

-- | Verify that the 'Decoder' is complete.
--
-- If the 'Decoder'\'s buffer still has data in it, 'NotDone' is returned.
finalizeDecoder :: Decoder -> Either DecodeFailure ()
finalizeDecoder Decoder{..}
    | decoderState /= Initial || not (Buffer.null decoderBuffer)
    = throwError NotDone
    | otherwise
    = return ()

-- | Decompress/decode as many frames as possible with the data in the
-- 'Decoder'.
--
-- /O(1)/ if there are insufficient bytes in the buffer.
decodeBuffered :: Decoder -> Either DecodeFailure DecodeResult
decodeBuffered =
    go []
  where
    go :: [Strict.ByteString] -> Decoder -> Either DecodeFailure DecodeResult
    go acc (Decoder b state@Initial) =
        case Buffer.splitExactly 4 b of
          Right (headerBs, rest) -> do
            header <- decodeHeader headerBs
            go acc (Decoder rest (KnownHeader header))
          Left _ ->
            return $ DecodeResult (reverse acc) $ Decoder b state
    go acc (Decoder b state@(KnownHeader header)) =
        case Buffer.splitExactly (frameHeaderPayloadSize header) b of
          Right (payloadBs, rest) -> do
            uncompressed <- decodePayload header payloadBs
            go (maybe acc (: acc) uncompressed) (Decoder rest Initial)
          Left _ ->
            return $ DecodeResult (reverse acc) $ Decoder b state

-- | Decompress/decode as many frames as possible from the data in the
-- 'Decoder'.
--
-- This is slightly lazier than 'decodeBuffered', since evaluating the result to
-- WHNF does not require determining if any of the decoding will fail. However,
-- if a failure does occur, a runtime exception is thrown.
--
-- /O(1)/ if there are insufficient bytes in the buffer.
decodeBufferedLazy :: Decoder -> DecodeResult
decodeBufferedLazy =
    go []
  where
    go :: [Strict.ByteString] -> Decoder -> DecodeResult
    go acc (Decoder b state@Initial) =
        case Buffer.splitExactly 4 b of
          Right (headerBs, rest) ->
            let
              header = throwLeft $ decodeHeader headerBs
            in
              go acc (Decoder rest (KnownHeader header))
          Left _ ->
            DecodeResult (reverse acc) $ Decoder b state
    go acc (Decoder b state@(KnownHeader header)) =
        case Buffer.splitExactly (frameHeaderPayloadSize header) b of
          Right (payloadBs, rest) ->
            let
              uncompressed = throwLeft $ decodePayload header payloadBs
            in
              go (maybe acc (: acc) uncompressed) (Decoder rest Initial)
          Left _ ->
              DecodeResult (reverse acc) $ Decoder b state


-- | Decode header
--
-- __Precondition:__ The given 'Strict.ByteString' must be exactly 4 bytes
-- long.
decodeHeader :: Strict.ByteString -> Either DecodeFailure FrameHeader
decodeHeader bs =
    case BS.Strict.unpack bs of
      [bid, b1, b2, b3] ->
        let
          payloadLen = lEWord24BytesToInt (b1, b2, b3)
        in
          case bid of
            0xff -> return $ FrameHeader StreamId     payloadLen
            0xfd -> return $ FrameHeader Padding      payloadLen
            0x00 -> return $ FrameHeader Compressed   payloadLen
            0x01 -> return $ FrameHeader Uncompressed payloadLen
            fid
              | fid `elem` [ 0x02 .. 0x7f ] ->
                  return $ FrameHeader (ReservedUnskippable fid) payloadLen
              | fid `elem` [ 0x80 .. 0xfd ] ->
                  return $ FrameHeader (ReservedSkippable fid) payloadLen
              | otherwise ->
                  error $
                    printf
                      ( "FrameFormat.decodeHeader: " ++
                        "impossible frame identifier 0x%x"
                      )
                      fid
      _ ->
        error "FrameFormat.decodeHeader: precondition violated"
  where
    lEWord24BytesToInt :: (Word8, Word8, Word8) -> Int
    lEWord24BytesToInt (lsb, mid, msb) =
            fromIntegral msb `shiftL` 16
        .|. fromIntegral mid `shiftL` 8
        .|. fromIntegral lsb

-- | Decode payload
--
-- __Precondition:__ The given 'Strict.ByteString' is the payload associated
-- with the given 'FrameHeader'.
decodePayload :: FrameHeader -> Strict.ByteString -> Either DecodeFailure (Maybe Strict.ByteString)
decodePayload header bs =
    case frameHeaderIdentifier header of
      StreamId ->
        if bs == "sNaPpY" then
          return Nothing
        else
          throwError $ BadStreamId bs
      Compressed ->
        let
          !(checksumBs, rest) = BS.Strict.splitAt 4 bs
        in
          case Raw.decompress rest of
            Nothing -> throwError $ DecompressionError rest
            Just decompressed ->
              let
                decodedChecksum = Checksum.decode checksumBs
                computedChecksum = Checksum.calculate decompressed
              in
                if computedChecksum /= decodedChecksum then
                  throwError $
                    BadChecksum decompressed decodedChecksum computedChecksum
                else
                  return $ Just decompressed
      Uncompressed ->
        let
          !(checksumBs, uncompressed) = BS.Strict.splitAt 4 bs
          decodedChecksum = Checksum.decode checksumBs
          computedChecksum = Checksum.calculate uncompressed
        in
          if computedChecksum /= decodedChecksum then
            throwError $
              BadChecksum uncompressed decodedChecksum computedChecksum
          else
            return $ Just uncompressed
      Padding ->
        return Nothing
      ReservedUnskippable fid ->
        -- An unskippable reserved frame is one that has an important payload,
        -- but we don't know what it is so we can't decode it.
        throwError $ ReservedUnskippableFrameId fid
      ReservedSkippable _ ->
        return Nothing
