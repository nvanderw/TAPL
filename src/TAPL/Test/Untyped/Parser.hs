module TAPL.Test.Untyped.Parser (tests) where

import ClassyPrelude

import Text.Parsec
import Test.HUnit
import TAPL.Untyped.Expr
import TAPL.Untyped.Parser

parseExpr :: ByteString -> Either ParseError Expr
parseExpr = parse expr "test"

tests :: Test
tests = "Parser" ~: [
    ("Identifier" ~:) $ case parseExpr "x" of
        Left err -> assertFailure . show $ err
        Right tree -> (EIdent "x") @=? tree,

    ("Lambda" ~:) $ case parseExpr "\\x.[x]" of
        Left err -> assertFailure . show $ err
        Right tree -> (ELambda "x" (EIdent "x")) @=? tree,


    "Application" ~: [
        case parseExpr "(x y)" of
            Left err -> assertFailure . show $ err
            Right tree -> (EApp (EIdent "x") (EIdent "y")) @=? tree,

        case parseExpr "(x)" of
            Left _ -> return ()
            Right _ -> assertFailure "Parsing should fail on unary application"]]
