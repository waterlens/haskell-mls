module Main (main) where

import GHC.Driver.Ppr (showSDoc)
import GHC.Utils.Outputable (SDoc, defaultSDocContext, renderWithContext)
import Lib (TranspileErr (..), parse, pprParsed, transpile)
import qualified Options.Applicative as OA
import qualified System.IO as IO
import qualified System.IO.Strict as SIO

newtype TranspilerArgs = TranspilerArgs
  { files :: [FilePath]
  }
  deriving (Show)

parseTranspilerArgs :: OA.Parser TranspilerArgs
parseTranspilerArgs =
  TranspilerArgs
    <$> OA.many
      ( OA.strArgument $
          OA.metavar "FILENAME"
            <> OA.help "Input file(s)"
      )

parserInfo :: OA.ParserInfo TranspilerArgs
parserInfo = OA.info (OA.helper <*> parseTranspilerArgs) OA.fullDesc

readUTF8File :: FilePath -> IO String
readUTF8File fp =
  IO.withFile fp IO.ReadMode $ \h -> do
    IO.hSetEncoding h IO.utf8
    SIO.hGetContents h

render :: SDoc -> String
render = renderWithContext defaultSDocContext

process :: FilePath -> IO ()
process path = do
  content <- readUTF8File path
  let mm = parse content
  case mm of
    Left (ParseErr e) -> do
      putStr "Unable to parse haskell file: "
      putStrLn $ render e
    Left (TransformErr e) -> do
      putStr "Unable to transpile haskell file: "
      putStrLn e
    Right m -> do
      _ <- transpile m
      return ()

main :: IO ()
main = do
  args <- OA.execParser parserInfo
  mapM_ process $ files args
