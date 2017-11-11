module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Linear.Vector
import Linear.V3
import Linear.Affine
import Linear.Metric

import Lib

main :: IO ()
main = do
  defaultMain (testGroup "Orbit tests" [unitTests, propertyChecks])

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "bodyDistance sun earth"
    $ bodyDistance sun earth @?= astronomicalUnit
  , testCase "bodyDistance earth moon"
    $ bodyDistance earth moon @?= 384399e3
  , testCase "selectOctant sun earth"
    $ selectOctant (_bodyPosition sun) earth @?= SED
  , testCase "subOctree for an Empty Region"
    $ let r = Region (P $ V3 0 0 0) (P $ V3 0 0 0) 1 4
          r' = Region (P $ V3 1 1 1) (P $ V3 0 0 0) 1 2
      in
      subOctree r NEU @?= Empty r'
  ]

propertyChecks :: TestTree
propertyChecks = testGroup "Property tests (QuickCheck)"
  [
  ]
