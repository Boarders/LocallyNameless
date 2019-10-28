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
properties =
  testGroup "Property Tests"
    $ locallyNamelessProperties <> churchProperties
  
locallyNamelessProperties :: [TestTree]
locallyNamelessProperties =
   [ QC.testProperty
     "fromLocallyNameless ∘ toLocallyNameless === id"
     (withMaxSuccess 1000 $ fromLocallyNamelessLeftInverse @String)
  ]

churchProperties :: [TestTree]
churchProperties = 
  [ QC.testProperty
      "Addition of Church numerals is commutative"
     (withMaxSuccess 100 additionIsCommutative)
  , QC.testProperty
      "Multiplication of Church numerals is commutative"
     (withMaxSuccess 100 multiplicationIsCommutative)
  ]
unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ HU.testCase
      "two plus two is four" $
       nf (churchAdd .$ cTwo .$ cTwo) @?= cFour
  , HU.testCase
      "two times three is six" $
       nf (churchMult .$ cTwo .$ cThree) @?= cSix
  , HU.testCase
      "two times three is six" $
       nf (churchMult .$ cTwo .$ cThree) @?= cSix
  ]
  
fromLocallyNamelessLeftInverse :: (Ord a, Show a) => Term a -> Property
fromLocallyNamelessLeftInverse e =
  (fromLocallyNameless . toLocallyNameless) e === e

additionIsCommutative :: Nat -> Nat -> Property
additionIsCommutative n m =
      nf (churchAdd .$ fromNat n .$ fromNat m)
  === nf (churchAdd .$ fromNat m .$ fromNat n)


multiplicationIsCommutative :: Nat -> Nat -> Property
multiplicationIsCommutative n m =
      nf (churchMult .$ fromNat n .$ fromNat m)
  === nf (churchMult .$ fromNat m .$ fromNat n)
  
