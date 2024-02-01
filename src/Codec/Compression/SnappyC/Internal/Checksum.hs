-- | Intended for qualified import:
--
-- > import Codec.Compression.SnappyC.Internal.Checksum (Checksum)
-- > import Codec.Compression.SnappyC.Internal.Checksum qualified as Checksum

module Codec.Compression.SnappyC.Internal.Checksum
  ( -- * Type
    Checksum -- Opaque

    -- ** Calculating checksums
  , calculate

    -- ** Encoding and decoding checksums
  , encode
  , decode
  ) where

import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString qualified as BS.Strict

import Data.Bits
import Data.Digest.CRC32C
import Data.Word

-- | Masked CRC32C checksum
newtype Checksum = Checksum { getChecksum :: Word32 }
  deriving newtype (Show, Eq)

-- | Calculate
calculate :: Strict.ByteString -> Checksum
calculate =
    Checksum . maskChecksum . crc32c
  where
    maskChecksum :: Word32 -> Word32
    maskChecksum = (+ 0xa282ead8) . (`rotateR` 15)

-- | Encode
encode :: Checksum -> Strict.ByteString
encode =
    BS.Strict.pack . toLittleEndian . getChecksum
  where
    toLittleEndian :: Word32 -> [Word8]
    toLittleEndian s =
      [ fromIntegral          (s .&. 0x000000ff)
      , fromIntegral $ shiftR (s .&. 0x0000ff00) 8
      , fromIntegral $ shiftR (s .&. 0x00ff0000) 16
      , fromIntegral $ shiftR (s .&. 0xff000000) 24
      ]

-- | Decode
--
-- __Precondition:__ Input must be exactly 4 bytes.
decode :: Strict.ByteString -> Checksum
decode =
    Checksum . fromLittleEndian . BS.Strict.unpack
  where
    fromLittleEndian :: [Word8] -> Word32
    fromLittleEndian [b1, b2, b3, b4] =
            fromIntegral b1
        .|. fromIntegral b2 `shiftL` 8
        .|. fromIntegral b3 `shiftL` 16
        .|. fromIntegral b4 `shiftL` 24
    fromLittleEndian _ = error "Checksum.decode: precondition violated"
