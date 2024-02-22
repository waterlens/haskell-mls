module Main(main) where

import GHC
import GHC.Driver.Monad (liftIO)
import GHC.Driver.Phases (Phase (..))
import GHC.Paths (libdir)
import GHC.Types.SourceFile (HscSource (..))
import GHC.Utils.Outputable (SDoc, defaultSDocContext, ppr, renderWithContext)
import qualified Options.Applicative as OA
import GHC.Core.Ppr (pprCoreBindings)
import GHC.Plugins (ModGuts(mg_binds))

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

render :: SDoc -> String
render = renderWithContext defaultSDocContext

process :: FilePath -> IO ()
process path = do
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags
    let uid = homeUnitId_ dflags
    addTarget
      Target
        { targetId = TargetFile path (Just $ Hsc HsSrcFile),
          targetAllowObjCode = False,
          targetUnitId = uid,
          targetContents = Nothing
        }
    _ <- load LoadAllTargets
    modSum <- getModSummary (mkModuleName "Main")
    p <- parseModule modSum
    t <- typecheckModule p
    d <- desugarModule t
    let cm = mg_binds $ coreModule d
    liftIO . putStrLn . render . ppr $ cm

process2 :: FilePath -> IO ()
process2 path = do
  runGhc (Just libdir) $ do
    cm <- compileToCoreModule path
    let cp = cm_binds cm
    liftIO . putStrLn . render . pprCoreBindings $ cp

main :: IO ()
main = do
  args <- OA.execParser parserInfo
  mapM_ process $ files args