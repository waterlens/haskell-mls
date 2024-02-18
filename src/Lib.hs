module Lib
  ( pprParsed,
    TranspileErr (..),
  )
where

import GHC.Driver.Session (DynFlags, defaultDynFlags)
import GHC.Hs.Extension (GhcPs)
import GHC.Parser.Lexer (ParseResult (..), getPsErrorMessages)
import GHC.Types.Error (NoDiagnosticOpts (NoDiagnosticOpts))
import GHC.Types.SrcLoc (Located, GenLocated (L))
import GHC.Utils.Error (pprMessages)
import GHC.Utils.Outputable (Outputable (ppr), SDoc)
import Language.Haskell.GhclibParserEx.GHC.Parser (parseModule)
import Language.Haskell.GhclibParserEx.GHC.Settings.Config (fakeSettings)
import Language.Haskell.Syntax (HsModule (..))

data TranspileErr = TransformErr String
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

transpile :: Module -> Either TranspileErr Module
transpile m = case m of
  L _ m' -> undefined

pprParsed :: String -> Either TranspileErr SDoc
pprParsed content = do
  m <- parse content
  return $ ppr m