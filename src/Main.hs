module Main where

import ClassyPrelude

import Test.HUnit
import TAPL.Test

main :: IO ()
main = void $ runTestTT tests
