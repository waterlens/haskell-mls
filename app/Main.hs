module Main (main) where

import Lib (pprParsed, TranspileErr (..))
import qualified Options.Applicative as OA
import qualified System.IO as IO
import qualified System.IO.Strict as SIO
import GHC.Utils.Outputable (renderWithContext, defaultSDocContext, pprHsString, IsLine (ftext))
import GHC.Data.FastString (fsLit)
import GHC.Driver.Ppr (showSDocUnsafe, showSDoc)

newtype TranspilerArgs = TranspilerArgs
  { files :: [FilePath]
  } deriving (Show)

parseTranspilerArgs :: OA.Parser TranspilerArgs
parseTranspilerArgs = TranspilerArgs
  <$> OA.some (OA.strArgument $
    OA.metavar "FILENAME" <>
    OA.help    "Input file(s)")

parserInfo :: OA.ParserInfo TranspilerArgs
parserInfo = OA.info (OA.helper <*> parseTranspilerArgs) OA.fullDesc
readUTF8File :: FilePath -> IO String
readUTF8File fp =
  IO.withFile fp IO.ReadMode $ \h -> do
    IO.hSetEncoding h IO.utf8
    SIO.hGetContents h

process :: FilePath -> IO ()
process path = do
  content <- readUTF8File path
  let render = renderWithContext defaultSDocContext
  case pprParsed content of
    Left (ParseErr e) -> do
      putStr "Unable to parse haskell file: "
      putStrLn $ render e
    Left (TransformErr e) -> do
      putStr "Unable to transpile haskell file: "
      putStrLn e
    Right m -> putStrLn $ render m

main :: IO ()
main = do
  args <- OA.execParser parserInfo
  mapM_ process $ files args
