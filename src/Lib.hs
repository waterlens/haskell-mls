module Lib
  ( pprParsed,
    parse,
    transpile,
    TranspileErr (..),
  )
where

import GHC.Driver.Session (DynFlags, defaultDynFlags)
import GHC.Hs.Extension (GhcPs)
import GHC.Parser.Lexer (ParseResult (..), getPsErrorMessages)
import GHC.Types.Error (NoDiagnosticOpts (NoDiagnosticOpts))
import GHC.Types.SrcLoc (Located, unLoc)
import GHC.Utils.Error (pprMessages)
import GHC.Utils.Outputable (Outputable (ppr), SDoc)
import Language.Haskell.GhclibParserEx.GHC.Parser (parseModule)
import Language.Haskell.GhclibParserEx.GHC.Settings.Config (fakeSettings)
import Language.Haskell.Syntax (HsModule (..))
import Language.Haskell.Syntax.Decls (HsDecl (..))

data TranspileErr
  = TransformErr String
  | ParseErr SDoc

type Module = Located (HsModule GhcPs)

baseDynFlags :: DynFlags
baseDynFlags = defaultDynFlags fakeSettings

parse :: String -> Either TranspileErr Module
parse content = do
  let pr = parseModule content baseDynFlags
  case pr of
    POk _ m -> Right m
    PFailed ps -> Left . ParseErr . pprMessages NoDiagnosticOpts $ getPsErrorMessages ps

printDecl :: HsDecl GhcPs -> IO ()
printDecl = t
  where
    t (SigD _ _decl) = putStrLn "SigD"
    t (TyClD _ _decl) = putStrLn "TyClD"
    t (ValD _ _decl) = putStrLn "ValD"
    t (DefD _ _decl) = putStrLn "DefD"
    t _ = putStrLn "<Other>"

transpile :: Module -> IO (Either TranspileErr ())
transpile m' =
  do
    case hsmodName m of
      Just _ -> return $ Left . TransformErr $ "not supported: module"
      _ -> do
        let decls = map unLoc $ hsmodDecls m
        mapM_ printDecl decls
        return $ Right ()
  where
    m = unLoc m'

pprParsed :: String -> Either TranspileErr SDoc
pprParsed content = do
  m <- parse content
  return $ ppr m