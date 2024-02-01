-- | Demo CLI application that uses the snappy-c incremental API with conduits
-- to compress and decompress command line input.

module Main where

import Codec.Compression.SnappyC.Framed qualified as Framed

import Conduit
import Control.Exception
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.Default
import Data.Maybe
import Options.Applicative

main :: IO ()
main =
    execParser opts >>= runSnappyCLI
  where
    opts :: ParserInfo SnappyCommand
    opts =
        info
          ( snappyCommand <|> roundTripCommand <**> helper )
          ( header "snappy-cli - snappy (de)compression on the command line"
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
        let result =
              if decompress then
                Framed.decompressLazy inp
              else
                Framed.compress inp
        BS.Lazy.writeFile output result


    goConduit :: IO ()
    goConduit =
        runConduitRes $
             sourceFileBS input
          .| (if decompress then decompressorC else compressorC)
          .| sinkFileBS output

runSnappyCLI (RoundTripCommand srcFile outFile) = do
    src <- BS.Lazy.readFile srcFile
    BS.Lazy.writeFile outFile (Framed.decompressLazy $ Framed.compress src)

data SnappyCommand =
      RoundTripCommand
        { input :: FilePath
        , output :: FilePath
        }
    | SnappyCommand
        { decompress :: Bool
        , format :: SnappyFormat
        , input :: FilePath
        , output :: FilePath
        , useConduit :: Bool
        }

data SnappyFormat = RawFormat | FrameFormat

roundTripCommand :: Parser SnappyCommand
roundTripCommand =
    subparser
      (   internal
       <> command "roundtrip"
            ( info
                (RoundTripCommand <$> inputOpt <*> outputOpt)
                (progDesc "compress then decompress")
            )
      )

snappyCommand :: Parser SnappyCommand
snappyCommand =
    SnappyCommand
      <$> switch
            (    long "decompress"
              <> short 'd'
              <> help "Decompress the input"
            )
      <*> ( fromMaybe FrameFormat <$>
            optional
              ( option
                  ( eitherReader $
                      \case
                        "raw" -> pure RawFormat
                        "framed" -> pure FrameFormat
                        f -> Left $ "failed to parse format: " ++ f
                  )
                  (    long "format"
                    <> short 't'
                    <> metavar "FORMAT"
                    <> help
                         ( "Compress to or decompress from FORMAT " ++
                           "(\"framed\" or \"raw\") (default: framed)"
                         )
                  )
              )
          )
      <*> inputOpt
      <*> outputOpt
      <*> switch
            (    long "conduit"
              <> help "Use the conduit interface"
            )

inputOpt :: Parser FilePath
inputOpt =
    option str
      (    short 'i'
        <> long "input"
        <> metavar "FILE"
        <> help "Take input from FILE"
      )

outputOpt :: Parser FilePath
outputOpt =
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
  => ConduitT
       Strict.ByteString
       Strict.ByteString
       m
       ()
decompressorC =
    go initialDecoder
  where
    initialDecoder  = Framed.initializeDecoder

    go :: Framed.Decoder -> ConduitT Strict.ByteString Strict.ByteString m ()
    go decoder = do
      mChunk <- await
      case mChunk of
        Just chunk -> do
          case Framed.decompressStep decoder chunk of
            Right (decompressed, decoder') -> do
              yieldMany decompressed
              go decoder'
            Left failure -> liftIO $ throwIO failure
        Nothing ->
          case Framed.finalizeDecoder decoder of
            Right ()     -> return ()
            Left failure -> liftIO $ throwIO failure
