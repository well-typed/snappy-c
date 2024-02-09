-- | Demo CLI application that uses the snappy-c incremental API with conduits
-- to compress and decompress command line input.

module Main where

import Codec.Compression.SnappyC.Framed qualified as Framed

import Conduit
import Control.Exception
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.Default
import Options.Applicative

main :: IO ()
main =
    execParser opts >>= runSnappyCLI
  where
    opts :: ParserInfo SnappyCommand
    opts =
        info
          ( snappyCommandP <**> helper )
          ( header "snappy-cli - Snappy (de)compression on the command line"
          )

runSnappyCLI :: SnappyCommand -> IO ()
runSnappyCLI SnappyCommand{..} = do
    if useConduit then
      goConduit
    else
      go
  where
    go :: IO ()
    go = do
        inp <- BS.Lazy.readFile input
        let f = case mode of
                  Compress ->
                    Framed.compress
                  Decompress ->
                    Framed.decompressWithParams
                      def { Framed.verifyChecksum = verify }
                  RoundTrip ->
                      Framed.decompressWithParams
                        def { Framed.verifyChecksum = verify }
                    . Framed.compress
        BS.Lazy.writeFile output (f inp)

    goConduit :: IO ()
    goConduit =
        runConduitRes $
             sourceFileBS input
          .| case mode of
               Compress   -> compressorC
               Decompress -> decompressorC verify
               RoundTrip  -> compressorC .| decompressorC verify
          .| sinkFileBS output

data SnappyCommand =
      SnappyCommand
        { mode :: Mode
        , verify :: Bool
        , input :: FilePath
        , output :: FilePath
        , useConduit :: Bool
        }

data Mode = Compress | Decompress | RoundTrip

snappyCommandP :: Parser SnappyCommand
snappyCommandP =
    SnappyCommand
      <$> modeP
      <*> switch
            (    long "verify"
              <> help "Verify chucksums during decompression"
            )
      <*> inputP
      <*> outputP
      <*> switch
            (    long "conduit"
              <> help "Use the conduit (de)compressor"
            )

modeP :: Parser Mode
modeP =
    flag Compress Decompress
      (    long "decompress"
        <> short 'd'
        <> help "Decompress the input"
      )
    <|>
    flag' RoundTrip
      (    long "roundtrip"
        <> short 'r'
        <> help "Compress then decompress the input"
        <> internal
      )

inputP :: Parser FilePath
inputP =
    option str
      (    short 'i'
        <> long "input"
        <> metavar "FILE"
        <> help "Take input from FILE"
      )

outputP :: Parser FilePath
outputP =
    option str
      (    short 'o'
        <> long "output"
        <> metavar "FILE"
        <> help "Output to FILE"
      )

-------------------------------------------------------------------------------
-- Conduits
-------------------------------------------------------------------------------

-- | Compressor conduit
compressorC :: forall m.
     Monad m
  => ConduitT
       Strict.ByteString
       Strict.ByteString
       m
       ()
compressorC =
    yield streamId >> go initialEncoder
  where
    (streamId, initialEncoder) = Framed.initializeEncoder

    go :: Framed.Encoder -> ConduitT Strict.ByteString Strict.ByteString m ()
    go encoder = do
      mChunk <- await
      case mChunk of
        Just chunk -> do
          let !(compressed, encoder') = Framed.compressStep def encoder chunk
          yieldMany compressed
          go encoder'
        Nothing ->
          yieldMany $ Framed.finalizeEncoder def encoder

-- | Decompressor conduit
decompressorC :: forall m.
     MonadIO m
  => Bool
  -> ConduitT
       Strict.ByteString
       Strict.ByteString
       m
       ()
decompressorC v =
    go initialDecoder
  where
    initialDecoder  = Framed.initializeDecoder

    go :: Framed.Decoder -> ConduitT Strict.ByteString Strict.ByteString m ()
    go decoder = do
        mChunk <- await
        case mChunk of
          Just chunk -> do
            let (decompressed, decoder') =
                  Framed.decompressStep
                    def {Framed.verifyChecksum = v}
                    decoder
                    chunk
            yieldMany decompressed
            go decoder'
          Nothing ->
            case Framed.finalizeDecoder decoder of
              Right ()     -> return ()
              Left failure -> liftIO $ throwIO failure
