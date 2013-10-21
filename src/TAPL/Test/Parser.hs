module TAPL.Test.Parser (allTests) where

import ClassyPrelude

import Text.Parsec
import Test.HUnit
import TAPL.Expr
import TAPL.Parser

parseExpr :: ByteString -> Either ParseError Expr
parseExpr = parse expr "test"

testIdent :: Test
testIdent = TestCase $ case parseExpr "x" of
    Left err -> assertFailure . show $ err
    Right tree -> (EIdent "x") @=? tree

testLambda :: Test
testLambda = TestCase $ case parseExpr "\\x.[x]" of
    Left err -> assertFailure . show $ err
    Right tree -> (ELambda "x" (EIdent "x")) @=? tree

testApp1 :: Test
testApp1 = TestCase $ case parseExpr "(x y)" of
    Left err -> assertFailure . show $ err
    Right tree -> (EApp (EIdent "x") (EIdent "y")) @=? tree

testApp2 :: Test
testApp2 = TestCase $ case parseExpr "(x)" of
    Left _ -> return ()
    Right _ -> assertFailure "Parsing should fail on unary application"

appTests :: Test
appTests = TestList [testApp1, testApp2]

allTests :: Test
allTests = TestList [testIdent, testLambda, appTests]
