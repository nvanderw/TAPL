module TAPL.Eval where

import ClassyPrelude

import TAPL.Expr

substitute :: Ident -> Val -> Expr -> Expr
substitute x v e = case e of
    EIdent y -> if x == y then ELit v else e
    ELambda y e' -> if x == y then e else ELambda y $ substitute x v e'
    EApp e1 e2 -> EApp (substitute x v e1) (substitute x v e2)
    ELit _ -> e
