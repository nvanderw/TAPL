module TAPL.SimplyTyped.Eval (typecheck) where

import ClassyPrelude
import TAPL.Untyped.Expr
import TAPL.SimplyTyped.Expr

type TypeEnv = [(Ident, Type)]

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just x) = Right x
maybeToEither x Nothing = Left x

-- |Typing relation Γ ⊢ e:τ, yielding an error message or an inferred type.
typecheck :: TypeEnv -> TExpr -> Either Text Type
typecheck env (TEIdent x) =
    maybeToEither ("Could not find type binding for " <> x) $
        lookup x env

typecheck env (TELambda x τ e) = do
    τ2 <- typecheck ((x, τ):env) e
    return $ TFunc τ τ2 -- τ -> τ2

typecheck env (TEApp e1 e2) = do
    τ1 <- typecheck env e1
    τ2 <- typecheck env e2
    case τ1 of
        TFunc τ2' τ3 -> if τ2 == τ2'
            then return τ3
            else Left $ "Expected " ++ (pretty τ2') ++ ", got " ++ (pretty τ2)
        _ -> Left $ "Expected function type instead of " ++ (pretty τ1)

typecheck _ (TELit (VInt _)) = return $ TBase BTInt
typecheck _ e = Left $ "Could not typecheck expression: " <> (pretty e)
