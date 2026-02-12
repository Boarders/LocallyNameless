{-# LANGUAGE TypeApplications #-}

module Main where

import LN.Expression
import LN.Church
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit      as HU
import Data.String (IsString)
import Data.Semigroup (Semigroup)
import Data.Set (member, notMember, insert, isSubsetOf)

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
     (withMaxSuccess 100 $ fromLocallyNamelessLeftInverse @String)
  , QC.testProperty
     "No variable capture during reduction"
     (withMaxSuccess 100 $ noCaptureDuringReduction)
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
  , HU.testCase
      "No variable capture: (λa. λb. a) b ≠ λb. b" $
       let term = App (Lam "a" (Lam "b" (Var "a"))) (Var "b")
           result = nf term
           wrongResult = Lam "b" (Var "b")
       in assertBool "Should NOT be alpha-equivalent to λb.b"
            (not (alphaEq result wrongResult))
  , HU.testCase
      "Bound variable adjustment: (λx. (λy. x) v) should reduce to λx. x" $
       let term = Lam "x" (App (Lam "y" (Var "x")) (Var "v"))
           expected = Lam "x" (Var "x")
       in nf term @?= expected
  ]
  
fromLocallyNamelessLeftInverse :: (Ord a, Show a, Increment a, IsString a, Semigroup a) => Term a -> Property
fromLocallyNamelessLeftInverse e =
  (fromLocallyNameless . toLocallyNameless) e === e

noCaptureDuringReduction :: Term String -> String -> Property
noCaptureDuringReduction t v =
  not (null v) &&  -- Filter out empty strings
  (fromLocallyNameless . toLocallyNameless) t == t ==>  -- Ensure term is valid
    let result = nf (App t (Var v))
        expectedFreeVars = insert v (freeVars t)
    in freeVars result `isSubsetOf` expectedFreeVars

additionIsCommutative :: Nat -> Nat -> Property
additionIsCommutative n m =
      nf (churchAdd .$ fromNat n .$ fromNat m)
  === nf (churchAdd .$ fromNat m .$ fromNat n)


multiplicationIsCommutative :: Nat -> Nat -> Property
multiplicationIsCommutative n m =
      nf (churchMult .$ fromNat n .$ fromNat m)
  === nf (churchMult .$ fromNat m .$ fromNat n)
  
