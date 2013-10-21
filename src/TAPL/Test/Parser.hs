module TAPL.Test.Parser where

import ClassyPrelude

import Text.Parsec
import Test.HUnit
import TAPL.Expr
import TAPL.Parser

testIdent :: Test
testIdent = TestCase $ do
    case parse expr "test" ("x" :: ByteString) of
        Left err -> assertFailure . show $ err
        Right tree -> (EIdent "x") @=? tree
