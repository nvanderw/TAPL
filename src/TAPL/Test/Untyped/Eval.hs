module TAPL.Test.Untyped.Eval (tests) where

import ClassyPrelude

import Test.HUnit
import TAPL.Untyped.Eval
import TAPL.Untyped.Expr

tests :: Test
tests = "Eval" ~: [
    "Substitute" ~: [
        ("Ident" ~:) $
            let one = VInt 1 in
            (substitute "x" one (EIdent "x")) @?= ELit one,

        "Lambda" ~: [
            -- Shadowing case
            let one    = VInt 1
                lambda = ELambda "x" (EIdent "x") in
            (substitute "x" one lambda) @?= lambda,

            -- Non-shadowing case
            let one = VInt 1
                lambda = ELambda "y" (EIdent "x") in
            (substitute "x" one lambda) @?= (ELambda "y" (ELit one))],

        ("Application" ~:) $
            let x = EIdent "x"
                zero = VInt 0
                ezero = ELit zero in
            (substitute "x" zero $ EApp x x) @?= (EApp ezero ezero),

        ("Literal" ~:) $ let e = ELit (VInt 1) in (substitute "x" (VInt 0) e) @?= e]]
