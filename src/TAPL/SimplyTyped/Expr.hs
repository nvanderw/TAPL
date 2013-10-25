module TAPL.SimplyTyped.Expr where

import ClassyPrelude
import TAPL.Untyped.Expr

data BaseType = BTInt deriving (Read, Show, Eq, Ord)

data Type = TBase BaseType
          | TFunc Type Type -- Function A -> B
          deriving (Read, Show, Eq, Ord)

-- |Abstract syntax of the typed lambda calculus
data TExpr = TEIdent Ident
           | TELambda Ident Type TExpr -- λx:τ.e
           | TEApp TExpr TExpr
           | TELit Val
           deriving (Read, Show, Eq, Ord)

instance Erase TExpr where
    erase (TEIdent x) = EIdent x
    erase (TELambda x _ e) = ELambda x (erase e)
    erase (TEApp e1 e2) = EApp (erase e1) (erase e2)
    erase (TELit v) = ELit v
