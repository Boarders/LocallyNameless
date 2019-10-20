{-# LANGUAGE TypeApplications #-}

module Main where

import LN.Expression
import LN.Church
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit      as HU

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]


properties :: TestTree
properties = testGroup "Property Tests"
  [QC.testProperty
     "fromLocallyNameless ∘ toLocallyNameless === id"
     (withMaxSuccess 1000 $ fromLocallyNamelessLeftInverse @String)
  ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ HU.testCase
      "two plus two is four" $
      (nf $ (churchAdd .$ cTwo) .$ cTwo) @?= cFour
  , HU.testCase
      "two times three is six" $
      (nf $ churchMult .$ cTwo .$ cThree) @?= cSix
  ]
  
fromLocallyNamelessLeftInverse :: (Ord a, Show a) => Term a -> Property
fromLocallyNamelessLeftInverse e =
  (fromLocallyNameless . toLocallyNameless) e === e
  

