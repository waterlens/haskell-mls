module Lib
  ( TranspileErr (..),
  )
where

import GHC.Hs.Extension (GhcPs)
import GHC.Utils.Outputable (SDoc)
import Language.Haskell.Syntax.Decls (HsDecl (..))
import GHC.Core (CoreBind, CoreProgram, CoreExpr, Expr (..))
import Ast (Statement, TypingUnit, Term)

data TranspileErr
  = TransformErr String
  | ParseErr SDoc

transpile :: CoreProgram -> IO TypingUnit
transpile = undefined

transpileCoreBind :: CoreBind -> IO [Statement]
transpileCoreBind = undefined

transpileExpr :: CoreExpr -> IO Term
transpileExpr expr = case expr of
  _ -> undefined

printDecl :: HsDecl GhcPs -> IO ()
printDecl = t
  where
    t (SigD _ _decl) = putStrLn "SigD"
    t (TyClD _ _decl) = putStrLn "TyClD"
    t (ValD _ _decl) = putStrLn "ValD"
    t (DefD _ _decl) = putStrLn "DefD"
    t _ = putStrLn "<Other>"
