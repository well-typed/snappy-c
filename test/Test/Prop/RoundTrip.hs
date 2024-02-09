module Test.Prop.RoundTrip
  ( tests
  ) where

import Test.Prop.Orphans ()

import Codec.Compression.SnappyC.Raw qualified as Raw
import Codec.Compression.SnappyC.Framed

import Control.Monad
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString qualified as BS.Strict
import Data.ByteString.Lazy qualified as BS.Lazy
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
    localOption (QuickCheckTests 1000) $
      testGroup "Test.Prop.RoundTrip"
        [  testProperty "rawRoundTripId" $
              sizeTable prop_rawRoundTripId
        ,  testProperty "framedRoundTripId" $
              sizeTable prop_framedRoundTripId
        ,  testProperty "framedChunkSizeRoundTripId" $
              chunkSizeTable prop_framedChunkSizeRoundTripId
        ,  testProperty "framedSlicedRoundTripId" $
              prop_framedSlicedRoundTripId
        ]

-- | Roundtripping raw compression and decompression is identity
prop_rawRoundTripId :: Strict.ByteString -> Bool
prop_rawRoundTripId src =
    (Just src ==) $ Raw.decompress (Raw.compress src)

-- | Roundtripping framed compression and decompression is identity
prop_framedRoundTripId :: Strict.ByteString -> Bool
prop_framedRoundTripId src =
    src == roundTripped
  where
    roundTripped =
          BS.Lazy.toStrict
        . decompress
        . compress
        $ BS.Lazy.fromStrict src

-- | The @decompress . compress@ identity holds for arbitrary compression chunk
-- sizes.
prop_framedChunkSizeRoundTripId :: EncodeParams -> Strict.ByteString -> Bool
prop_framedChunkSizeRoundTripId eps src =
    let
      (compressedChunks, e') = compressStep eps initialEncoder src
      compressedChunksRest   = finalizeEncoder eps e'

      roundTripped =
          BS.Lazy.toStrict
        . decompress
        . BS.Lazy.fromChunks
        $ streamId : compressedChunks <> compressedChunksRest
    in
      src == roundTripped
  where
    (streamId, initialEncoder) = initializeEncoder

-- | The @decompress . compress@ identity holds for arbitrary slicing of
-- compression and decompression input.
prop_framedSlicedRoundTripId ::
     EncodeParams
  -> Slices
  -- ^ Compression input slices
  -> Slices
  -- ^ Decompression input slices
  -> Strict.ByteString
  -> Bool
prop_framedSlicedRoundTripId eps compressSlices decompressSlices src =
    let
      compressed =
          BS.Lazy.toStrict
        . compressWithParams eps
        . BS.Lazy.fromChunks
        $ slice compressSlices src

      roundTripped =
          BS.Lazy.toStrict
        . decompress
        . BS.Lazy.fromChunks
        $ slice decompressSlices compressed
    in
      src == roundTripped


-------------------------------------------------------------------------------
-- Auxiliary
-------------------------------------------------------------------------------

data Slices = Slices [Int]
  deriving Show

-- | Slices the input into chunks the size of each of the given 'Slices'
slice :: Slices -> Strict.ByteString -> [Strict.ByteString]
slice (Slices []) bs     = [bs]
slice _           ""     = []
slice (Slices (s:ss)) bs =
    let (sliced, rest) = BS.Strict.splitAt s bs
    in  sliced : slice (Slices ss) rest

instance Arbitrary Slices where
  arbitrary = do
      slices <- replicateM 100
        $ frequency
            [ (9, choose (0   , 1000))
            , (1, choose (1000, 1_000_000))
            ]
      return $ Slices slices

  shrink (Slices slices) = [ Slices slices' | slices' <- shrink slices ]

sizeTable ::
     Testable prop
  => (Strict.ByteString -> prop)
  -> Strict.ByteString
  -> Property
sizeTable f bs =
    tabulate "size"
      [ "10^" ++ show (sizeBracket (BS.Strict.length bs))
      ]
      (f bs)
  where
    sizeBracket :: Int -> Int
    sizeBracket 0 = 1
    sizeBracket x = floor @Double . logBase 10 $ fromIntegral x

chunkSizeTable ::
     Testable prop
  => (EncodeParams -> prop)
  -> EncodeParams
  -> Property
chunkSizeTable f inp@(EncodeParams fs _) =
    tabulate "frame size"
      [ "10^" ++ show (sizeBracket (unFrameSize fs))
      ]
      (f inp)
  where
    sizeBracket :: Int -> Int
    sizeBracket 0 = 1
    sizeBracket x = floor @Double . logBase 10 $ fromIntegral x
