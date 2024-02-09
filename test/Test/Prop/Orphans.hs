{-# OPTIONS_GHC -Wno-orphans #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use ++" #-}

module Test.Prop.Orphans where

import Codec.Compression.SnappyC.Framed

import Control.Monad
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString qualified as BS.Strict

import Test.Tasty.QuickCheck

instance Arbitrary Strict.ByteString where
  arbitrary = do
      n <-
        frequency
          [ (9, choose (0   , 1000))
          , (1, choose (1000, 1_000_000))
          ]
      BS.Strict.pack <$> replicateM n arbitrary
  shrink bs =
      BS.Strict.pack <$> shrink (BS.Strict.unpack bs)

instance Arbitrary EncodeParams where
  arbitrary =
          EncodeParams
      <$> arbitrary
      <*> arbitrary

  shrink (EncodeParams fs r) =
      concat
        [ [ EncodeParams fs' r
          | fs' <- shrink fs
          ]
        , [ EncodeParams fs r'
          | r' <- shrink r
          ]
        ]

instance Arbitrary FrameSize where
  arbitrary = do
      n <-
        oneof
          [ pure 1
          , pure 2
          , pure 65535
          , pure 65536
          , choose (1, 65536) -- The range of valid chunk sizes
          ]
      return $ customFrameSize n

  shrink n = [ customFrameSize m | m <- shrink (unFrameSize n), m >= 1 ]

instance Arbitrary Threshold where
  arbitrary = do
      oneof
        [ pure AlwaysCompress
        , pure NeverCompress

          -- According to Google, max Snappy compression ratio is about 4
          -- https://github.com/google/snappy?tab=readme-ov-file#performance
        , Ratio . (/ 8.0) <$> elements
            [ 7.0  -- Similar (equivalent?) to AlwaysCompress
            , 8.0
            ..
              32.0 -- Similar to NeverCompress
            ]
        ]

  shrink NeverCompress  = []
  shrink AlwaysCompress = [NeverCompress]
  shrink (Ratio r)
      | r < 7.0 / 8.0  = [ AlwaysCompress  ]
      | otherwise      = [ Ratio $ r - 1/8 ]