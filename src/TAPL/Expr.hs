module TAPL.Expr where

import ClassyPrelude
import GHC.Generics

type Ident = Text

data Expr = EIdent Ident
          | ELambda Ident Expr
          | EApp Expr Expr deriving (Read, Show, Eq, Ord, Generic)
