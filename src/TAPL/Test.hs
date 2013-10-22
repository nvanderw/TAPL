module TAPL.Test (tests) where

import Test.HUnit
import qualified TAPL.Test.Parser as Parser
import qualified TAPL.Test.Eval as Eval

tests :: Test
tests = "Test" ~: [Parser.tests, Eval.tests]
