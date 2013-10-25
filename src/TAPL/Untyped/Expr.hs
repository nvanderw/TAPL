module TAPL.Untyped.Expr where

import ClassyPrelude
import GHC.Generics

type Ident = Text

data Expr = EIdent Ident
          | ELambda Ident Expr
          | EApp Expr Expr
          | ELit Val deriving (Read, Show, Eq, Ord, Generic)

data Val = VInt Int | VLambda Ident Expr deriving (Read, Show, Eq, Ord, Generic)

class Pretty a where
    pretty :: a -> Text

instance Pretty Expr where
    pretty (EIdent x) = x
    pretty (ELambda x e) = "\\" <> x <> ".[" <> (pretty e) <> "]"
    pretty (EApp e1 e2) = "(" <> (pretty e1) <> " " <> (pretty e2) <> ")"
    pretty (ELit v) = pretty v

instance Pretty Val where
    pretty (VInt n) = pack . show $ n
    pretty (VLambda x e) = pretty $ ELambda x e

class Erase a where
    erase :: a -> Expr
