module TAPL.Untyped.Eval where

import ClassyPrelude

import TAPL.Untyped.Expr

substitute :: Ident -> Val -> Expr -> Expr
substitute x v e = case e of
    EIdent y -> if x == y then ELit v else e
    ELambda y e' -> if x == y then e else ELambda y $ substitute x v e'
    EApp e1 e2 -> EApp (substitute x v e1) (substitute x v e2)
    ELit _ -> e

-- |Step function with yields either an error message or an expression
step :: Expr -> Either Text Expr
step (EIdent x) = Left $ "Tried to evaluate free variable " <> x
step (ELambda x e) = return . ELit $ VLambda x e
step (EApp (ELit v1) (ELit v2)) = case v1 of
    (VLambda x e) -> return $ substitute x v2 e
    _ -> Left $ "Type error while reducing application"
step (EApp e1 (ELit v2)) = EApp <$> (step e1) <*> (return $ ELit v2)
step (EApp e1 e2) = EApp e1 <$> (step e2)
step e@(ELit _) = return e


eval :: Expr -> Either Text Val
eval e = case e of
    ELit v -> return v
    _ -> step e >>= eval
