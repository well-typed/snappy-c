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
import System.Random
import Control.Exception
import Data.Word

benchmarkMBs :: Int
benchmarkMBs = 2

main :: IO ()
main = do
    benchRandom          <- benchmarkOn "random"            [randomMBs benchmarkMBs         ]
    benchZero            <- benchmarkOn "zeros"             [return $ zeroMBs benchmarkMBs  ]
    benchWord            <- benchmarkOn "words"             [return $ wordMBs benchmarkMBs  ]
    benchInterleavedWord <- benchmarkOn "interleaved_words" [interleavedWordMBs benchmarkMBs]
    benchCombined <-
      benchmarkOn "combined"
        [ randomMBs benchmarkMBs
        , return $ zeroMBs benchmarkMBs
        , return $ wordMBs benchmarkMBs
        , interleavedWordMBs benchmarkMBs
        ]
    defaultMain
      [ benchRandom
      , benchZero
      , benchWord
      , benchInterleavedWord
      , benchCombined
      ]

benchmarkOn :: String -> [IO Lazy.ByteString] -> IO Benchmark
benchmarkOn label genInputs = do
    inps              <- evaluate . force =<< sequence genInputs
    compressed_snappy <- evaluate . force $ map SnappyC.compress inps
    compressed_zlib   <- evaluate . force $ map Zlib.compress    inps
    compressed_gzip   <- evaluate . force $ map GZip.compress    inps
    return $
      bgroup label
        [ bgroup "compression"
            [ bench "snappy-c"    $ nf (map SnappyC.compress   ) inps
            , bench "snappy-lazy" $ nf (map SnappyLazy.compress) inps
            , bench "zlib"        $ nf (map Zlib.compress      ) inps
            , bench "gzip"        $ nf (map GZip.compress      ) inps
            ]
        , bgroup "decompression"
            [ bench "snappy-c"    $ nf (map SnappyC.decompress   ) compressed_snappy
            , bench "snappy-lazy" $ nf (map SnappyLazy.decompress) compressed_snappy
            , bench "zlib"        $ nf (map Zlib.decompress      ) compressed_zlib
            , bench "gzip"        $ nf (map GZip.decompress      ) compressed_gzip
            ]
        , bgroup "roundtrip"
            [ bench "snappy-c"    $ nf (map $ SnappyC.decompress    . SnappyC.compress   ) inps
            , bench "snappy-lazy" $ nf (map $ SnappyLazy.decompress . SnappyLazy.compress) inps
            , bench "zlib"        $ nf (map $ Zlib.decompress       . Zlib.compress      ) inps
            , bench "gzip"        $ nf (map $ GZip.decompress       . GZip.compress      ) inps
            ]
        ]

randomMBs :: Int -> IO Lazy.ByteString
randomMBs n =
    BS.Lazy.pack <$> replicateM (n * mb) randomByte
  where
    randomByte :: IO Word8
    randomByte = randomIO

zeroMBs :: Int -> Lazy.ByteString
zeroMBs n = BS.Lazy.pack $ replicate (n * mb) 0

wordMBs :: Int -> Lazy.ByteString
wordMBs n =
      BS.Lazy.pack
    . concat
    $ replicate (n * mb `div` 8) repeatedWord

interleavedWordMBs :: Int -> IO Lazy.ByteString
interleavedWordMBs n =
        BS.Lazy.pack
      . concat
    <$> replicateM (n * mb `div` (8 * 2)) ((repeatedWord ++) <$> randomBytes)
  where
    randomBytes :: IO [Word8]
    randomBytes = replicateM 8 randomIO

repeatedWord :: [Word8]
repeatedWord = [0 .. 7]

mb :: Int
mb = 1024 * kb

kb :: Int
kb = 1024
