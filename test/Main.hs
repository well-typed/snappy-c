module Main where

import Codec.Compression.SnappyC

import Data.ByteString (ByteString)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances.ByteString ()

main :: IO ()
main =
    defaultMain $
      testGroup "Test.SnappyC"
        [ -- One million roundtrip tests
          testProperty "roundtripId" $
            withMaxSuccess 1_000_000 $
              property prop_compressDecompressId

          -- Ensure invalid decompression is Nothing
        , testCase "invalidDecompress" $ do
            Nothing @?= decompress "not a valid compressed string"
        ]

-- | Roundtripping compression and decompression is identity
prop_compressDecompressId :: ByteString -> Bool
prop_compressDecompressId src =
    case decompress (compress src) of
      Just roundtrip ->
        roundtrip == src
      Nothing ->
        False
