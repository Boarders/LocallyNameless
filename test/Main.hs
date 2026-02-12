{-# LANGUAGE TypeApplications #-}

module Main where

import LN.Expression
import LN.Church
import qualified Eval
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

maxSuccess :: Int
maxSuccess = 1000

properties :: TestTree
properties =
  testGroup "Property Tests"
    $ locallyNamelessProperties <> churchProperties <> unboundProperties

locallyNamelessProperties :: [TestTree]
locallyNamelessProperties =
   [ QC.testProperty
     "fromLocallyNameless ∘ toLocallyNameless === id"
     (withMaxSuccess maxSuccess $ fromLocallyNamelessLeftInverse @String)
  , QC.testProperty
     "No variable capture during reduction"
     (withMaxSuccess maxSuccess $ noCaptureDuringReduction)
  ]

churchProperties :: [TestTree]
churchProperties =
  [ QC.testProperty
      "Addition of Church numerals is commutative"
     (withMaxSuccess maxSuccess additionIsCommutative)
  , QC.testProperty
      "Multiplication of Church numerals is commutative"
     (withMaxSuccess maxSuccess multiplicationIsCommutative)
  ]

unboundProperties :: [TestTree]
unboundProperties =
  [ QC.testProperty
      "Normal form agrees with unbound-generics"
      (withMaxSuccess maxSuccess Eval.prop_nf_agrees)
  , QC.testProperty
      "Weak head normal form agrees with unbound-generics"
      (withMaxSuccess maxSuccess Eval.prop_whnf_agrees)
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
  , testGroup "Unbound-generics agreement tests"
      [ HU.testCase
          "Capture avoidance: (λx. λy. x) y agrees with unbound (check via UTerm aeq)" $
           let term = App (Lam "a" (Lam "b" (Var "a"))) (Var "b")
               lnResult = nf term
               lnAsU = Eval.fromLN lnResult
               uAsU = Eval.nfU (Eval.fromLN term)
           in if Eval.aeqU lnAsU uAsU
              then pure ()
              else assertFailure $
                "LN and unbound should be alpha-equivalent (via aeq)\n" ++
                "LN result: " ++ show lnResult ++ "\n" ++
                "LN as UTerm aeq U result: " ++ show (Eval.aeqU lnAsU uAsU)
      , HU.testCase
          "Nested capture: (λx. (λy. (λx. y) x)) y agrees with unbound" $
           let term = App (Lam "x" (Lam "y" (App (Lam "x" (Var "y")) (Var "x")))) (Var "y")
               lnResult = nf term
               lnAsU = Eval.fromLN lnResult
               uAsU = Eval.nfU (Eval.fromLN term)
           in if Eval.aeqU lnAsU uAsU
              then pure ()
              else assertFailure $
                "LN and unbound should be alpha-equivalent (via aeq)"
      , HU.testCase
          "Self-application with capture risk: (λx. x x) (λy. y) agrees" $
           let term = App (Lam "x" (App (Var "x") (Var "x"))) (Lam "y" (Var "y"))
               lnResult = nf term
               lnAsU = Eval.fromLN lnResult
               uAsU = Eval.nfU (Eval.fromLN term)
           in if Eval.aeqU lnAsU uAsU
              then pure ()
              else assertFailure $
                "LN and unbound should be alpha-equivalent (via aeq)"
      , HU.testCase
          "Naive non-capture avoiding substitutuion should disagree when comparing to unbound implementation" $
           -- Classic variable capture example: (λx. λy. x) y should NOT reduce to λy. y
           let term = App (Lam "x" (Lam "y" (Var "x"))) (Var "y")
               lnResult = nf term
               brokenResult = Eval.toLN (Eval.nfU_broken (Eval.fromLN term))
           in if not (alphaEq lnResult brokenResult)
              then pure ()
              else assertFailure $
                "Broken implementation should give wrong answer\n" ++
                "LN result (correct): " ++ show lnResult ++ "\n" ++
                "Broken result: " ++ show brokenResult
      ]
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
