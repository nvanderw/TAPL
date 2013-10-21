module Main where

import ClassyPrelude

import Test.HUnit
import TAPL.Test.Parser

main :: IO ()
main = void $ runTestTT testIdent
