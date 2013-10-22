module TAPL.Test.Parser (tests) where

import ClassyPrelude

import Text.Parsec
import Test.HUnit
import TAPL.Expr
import TAPL.Parser

parseExpr :: ByteString -> Either ParseError Expr
parseExpr = parse expr "test"

tests :: Test
tests = "Parser" ~: [
    ("Identifier" ~:) . TestCase $ case parseExpr "x" of
        Left err -> assertFailure . show $ err
        Right tree -> (EIdent "x") @=? tree,

    ("Lambda" ~:) . TestCase $ case parseExpr "\\x.[x]" of
        Left err -> assertFailure . show $ err
        Right tree -> (ELambda "x" (EIdent "x")) @=? tree,


    "Application" ~: [
        TestCase $ case parseExpr "(x y)" of
            Left err -> assertFailure . show $ err
            Right tree -> (EApp (EIdent "x") (EIdent "y")) @=? tree,

        TestCase $ case parseExpr "(x)" of
            Left _ -> return ()
            Right _ -> assertFailure "Parsing should fail on unary application"]]
