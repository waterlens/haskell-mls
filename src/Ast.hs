module Ast
  ( TypingUnit (..),
    Statement (..),
    TypeDefKind (..),
    Term (..),
    If (..),
    IfBody (..),
    Literal (..),
  )
where

import GHC.Data.FastString
import GHC.Utils.Outputable
  ( IsOutput (empty),
    Outputable (..),
    char,
    darrow,
    ftext,
    interpp'SP,
    nest,
    parens,
    pprHsString,
    text,
    ($+$),
    (<+>),
    (<>),
  )
import Prelude hiding ((<>))

newtype TypingUnit = TypingUnit [Statement]

data Statement
  = TypeDef {kind :: TypeDefKind, name :: FastString, params :: Maybe [FastString]}
  | FuncDef {name :: FastString, body :: Term}
  | TopTerm Term

data TypeDefKind = Class

data Literal
  = IntLit !Int
  | StrLit FastString

instance Outputable Literal where
  ppr (IntLit i) = ppr i
  ppr (StrLit i) = pprHsString i

data Term
  = Lit Literal
  | Var FastString
  | Lam [FastString] Term
  | App Term Term
  | Let FastString Term Term
  | Select Term FastString
  | Tuple [FastString]
  | Block [Term]
  | Match If

instance Outputable Term where
  ppr (Lit lit) = ppr lit
  ppr (Var fs) = ppr fs
  ppr (Lam xs e) = parens $ interpp'SP xs <+> darrow <+> ppr e
  ppr (App e1 t@(Tuple _)) = ppr e1 <> ppr t
  ppr (App e1 e2) = ppr e1 <> parens (ppr e2)
  ppr (Let x e1 e2) =
    (text "let" <+> ppr x <+> text "=" <+> ppr e1)
      $+$ ppr e2
  ppr (Select (Var v) fld) = ppr v <> char '.' <> ppr fld
  ppr (Select e fld) = parens (ppr e) <> char '.' <> ppr fld
  ppr (Tuple xs) = parens $ interpp'SP xs
  ppr (Block terms) = foldr (($+$) . ppr) empty terms
  ppr (Match ifs) = ppr ifs

data If = If IfBody (Maybe Term)

instance Outputable If where
  ppr (If e Nothing) =
    text "if" <+> ppr e
  ppr (If e (Just els)) =
    text "if"
      <+> ppr e
      $+$ text "else"
      $+$ nest 2 (ppr els)

data IfBody
  = IfThen Term Term
  | IfOpApp Term FastString IfBody
  | IfBlock [IfBody]

instance Outputable IfBody where
  ppr (IfThen cond t) = ppr cond <+> text "then" <+> nest 2 (ppr t)
  ppr (IfOpApp cond op e) = ppr cond <+> ftext op $+$ nest 2 (ppr e)
  ppr (IfBlock ifs) = foldr (($+$) . ppr) empty ifs
