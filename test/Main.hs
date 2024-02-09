{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

module Main where

import Test.Prop.RoundTrip qualified as RoundTrip

import Codec.Compression.SnappyC.Raw qualified as Raw
import Codec.Compression.SnappyC.Framed qualified as Framed

import Data.ByteString.Lazy qualified as BS.Lazy
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
    defaultMain $
      testGroup "Test.SnappyC"
        [ -- Round trip property tests
          RoundTrip.tests

          -- Sanity checks

          -- Ensure size of frame compressed output for empty input is exactly
          -- 10 bytes
        , testCase "emptyCompressedSize" $
            10 @?= BS.Lazy.length (Framed.compress "")

          -- Ensure invalid raw decompression is 'Nothing'
        , testCase "invalidRawDecompress" $
            Nothing @?= Raw.decompress "not a valid compressed string"

          -- Ensure invalid framed decompression throws an exception
        , testCase "invalidFramedDecompress" $
            case Framed.decompress' "not a valid compressed string" of
              Right _ -> assertFailure "invalid decompression succeeded"
              Left _  -> return ()
        ]
