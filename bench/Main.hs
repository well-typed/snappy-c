-- Prevents the 'megabytes' calls from being combined as a single CAF whose
-- evaluation cost is attributed to the first benchmark.
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Codec.Compression.SnappyC.Framed qualified as SnappyC
import Codec.Compression.Snappy.BSL qualified as SnappyLazy

import Codec.Compression.Zlib qualified as Zlib
import Codec.Compression.GZip qualified as GZip

import Control.DeepSeq
import Control.Monad
import Criterion.Main
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.ByteString.Lazy qualified as BS.Lazy
import System.IO.Unsafe
import System.Random

benchmarkMBs :: Int
benchmarkMBs = 50

main :: IO ()
main = do
    defaultMain
      [ bgroup "compression"
          [ bench "snappy-c"    $ nf SnappyC.compress    (megabytes benchmarkMBs)
          , bench "snappy-lazy" $ nf SnappyLazy.compress (megabytes benchmarkMBs)
          , bench "zlib"        $ nf Zlib.compress       (megabytes benchmarkMBs)
          , bench "gzip"        $ nf GZip.compress       (megabytes benchmarkMBs)
          ]
      , bgroup "decompression"
          [ bench "snappy-c"    $ nf SnappyC.decompressLazy snappy_compressed
          , bench "snappy-lazy" $ nf SnappyLazy.decompress  snappy_compressed
          , bench "zlib"        $ nf Zlib.decompress        zlib_compressed
          , bench "gzip"        $ nf GZip.decompress        gzip_compressed
          ]
      , bgroup "roundtrip"
          [ bench "snappy-c"    $ nf (SnappyC.decompressLazy . SnappyC.compress   ) (megabytes benchmarkMBs)
          , bench "snappy-lazy" $ nf (SnappyLazy.decompress  . SnappyLazy.compress) (megabytes benchmarkMBs)
          , bench "zlib"        $ nf (Zlib.decompress        . Zlib.compress      ) (megabytes benchmarkMBs)
          , bench "gzip"        $ nf (GZip.decompress        . GZip.compress      ) (megabytes benchmarkMBs)
          ]
      ]
  where
    !snappy_compressed = force $ SnappyC.compress (megabytes benchmarkMBs)
    !zlib_compressed   = force $ Zlib.compress    (megabytes benchmarkMBs)
    !gzip_compressed   = force $ GZip.compress    (megabytes benchmarkMBs)

{-# NOINLINE megabytes #-}
megabytes :: Int -> Lazy.ByteString
megabytes n = do
      BS.Lazy.pack
    . unsafePerformIO
    $ replicateM (n * mb) randomIO
  where
    mb = 1024 * kb
    kb = 1024
