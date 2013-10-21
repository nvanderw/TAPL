module Main where

import ClassyPrelude

import Test.HUnit
import qualified TAPL.Test.Parser as Parser

main :: IO ()
main = void $ runTestTT Parser.allTests
