{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Eval where

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Unbound.Generics.LocallyNameless
import qualified LN.Expression as LN
import Test.QuickCheck
import Data.String (IsString(..))

data UTerm = UVar (Name UTerm)
           | ULam (Bind (Name UTerm) UTerm)
           | UApp UTerm UTerm
  deriving (Show, Generic, Typeable)

instance Alpha UTerm
instance Subst UTerm UTerm where
  isvar (UVar v) = Just (SubstName v)
  isvar _        = Nothing

ulam :: String -> UTerm -> UTerm
ulam n body = ULam (bind (string2Name n) body)

instance IsString UTerm where
  fromString = UVar . string2Name

fromLN :: LN.Term String -> UTerm
fromLN (LN.Var n)    = UVar (string2Name n)
fromLN (LN.Lam n e)  = ULam (bind (string2Name n) (fromLN e))
fromLN (LN.App l r)  = UApp (fromLN l) (fromLN r)

-- Note: This conversion may produce alpha-variants, use LN.alphaEq for comparison
toLN :: UTerm -> LN.Term String
toLN (UVar n)    = LN.Var (name2String n)
toLN (ULam b)    = runFreshM $ do
  (n, body) <- unbind b
  return $ LN.Lam (name2String n) (toLN body)
toLN (UApp l r)  = LN.App (toLN l) (toLN r)

freeVarsU :: UTerm -> [Name UTerm]
freeVarsU (UVar n) = [n]
freeVarsU (UApp l r) = freeVarsU l ++ freeVarsU r
freeVarsU (ULam b) = runFreshM $ do
  (n, body) <- unbind b
  return $ filter (/= n) (freeVarsU body)

whnfU :: UTerm -> UTerm
whnfU term = runFreshM $ go 0 term []
  where
    maxSteps = 10000
    go :: Int -> UTerm -> [UTerm] -> FreshM UTerm
    go steps t as
      | steps > maxSteps = error $ "whnfU exceeded " ++ show maxSteps ++ " steps"
      | otherwise = case (t, as) of
          (UApp l r, args) ->
            go (steps + 1) l (r : args)
          (ULam b, a:args) -> do
            let argFVs = freeVarsU a
            (n, body) <- unbind b
            -- If the binder name appears free in the argument, rename it
            n' <- if n `elem` argFVs
                  then fresh n
                  else return n
            let body' = if n' /= n then subst n (UVar n') body else body
            let result = subst n' a body'
            go (steps + 1) result args
          _ ->
            return $ foldl UApp t as

nfU :: UTerm -> UTerm
nfU term = runFreshM $ go 0 term []
  where
    maxSteps = 10000
    go :: Int -> UTerm -> [UTerm] -> FreshM UTerm
    go steps t as
      | steps > maxSteps = error $ "nfU exceeded " ++ show maxSteps ++ " steps"
      | otherwise = case (t, as) of
          (UApp l r, args) ->
            go (steps + 1) l (r : args)
          (ULam b, []) -> do
            (n, body) <- unbind b
            body' <- go steps body []
            return $ ULam (bind n body')
          (ULam b, a:args) -> do
            let argFVs = freeVarsU a
            (n, body) <- unbind b
            -- If the binder name appears free in the argument, rename it
            n' <- if n `elem` argFVs
                  then fresh n
                  else return n
            let body' = if n' /= n then subst n (UVar n') body else body
            let result = subst n' a body'
            go (steps + 1) result args
          _ -> do
            as' <- mapM (\a -> go 0 a []) as
            return $ foldl UApp t as'

aeqU :: UTerm -> UTerm -> Bool
aeqU = aeq

-- | Incorrect non-capture avoiding substitution to check the above
-- test comparison test does something
naiveSubst :: Name UTerm -> UTerm -> UTerm -> UTerm
naiveSubst x s (UVar y)
  | y == x    = s
  | otherwise = UVar y
naiveSubst x s (UApp t1 t2) = UApp (naiveSubst x s t1) (naiveSubst x s t2)
naiveSubst x s (ULam b) = runFreshM $ do
  (y, body) <- unbind b
  -- doesn't check if y is free in s
  return $ ULam (bind y (naiveSubst x s body))

nfU_broken :: UTerm -> UTerm
nfU_broken term = go 0 term []
  where
    maxSteps = 10000
    go :: Int -> UTerm -> [UTerm] -> UTerm
    go steps t as
      | steps > maxSteps = error $ "nfU_broken exceeded " ++ show maxSteps ++ " steps"
      | otherwise = case (t, as) of
          (UApp l r, args) ->
            go (steps + 1) l (r : args)
          (ULam b, []) ->
            runFreshM $ do
              (n, body) <- unbind b
              return $ ULam (bind n (nfU_broken body))
          (ULam b, a:args) ->
            let body' = runFreshM $ do
                  (n, body) <- unbind b
                  return $ naiveSubst n a body
            in go (steps + 1) body' args
          _ ->
            foldl UApp t (map nfU_broken as)

prop_nf_agrees :: LN.Term String -> Property
prop_nf_agrees term =
  -- Only test on valid terms
  let term' = (LN.fromLocallyNameless . LN.toLocallyNameless) term
  in term == term' ==>
    let lnResult = LN.nf term
        uTerm = fromLN term
        uResult_UTerm = nfU uTerm
        uResult = toLN uResult_UTerm
        -- Double check: convert both back to UTerm and check aeq
        lnAsU = fromLN lnResult
        agrees = aeqU lnAsU uResult_UTerm
    in counterexample
        ("Original: " ++ show term ++ "\n" ++
         "LN result: " ++ show lnResult ++ "\n" ++
         "U result:  " ++ show uResult ++ "\n" ++
         "LN alpha eq: " ++ show (LN.alphaEq lnResult uResult) ++ "\n" ++
         "U  alpha eq: " ++ show agrees)
        (LN.alphaEq lnResult uResult && agrees)

prop_whnf_agrees :: LN.Term String -> Property
prop_whnf_agrees term =
  let term' = (LN.fromLocallyNameless . LN.toLocallyNameless) term
  in term == term' ==>
    let lnResult = LN.whnf term
        uResult = toLN (whnfU (fromLN term))
    in counterexample
        ("LN result: " ++ show lnResult ++ "\n" ++
         "U result:  " ++ show uResult ++ "\n" ++
         "Alpha eq:  " ++ show (LN.alphaEq lnResult uResult))
        (LN.alphaEq lnResult uResult)
