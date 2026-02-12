{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module LN.Expression where

import Data.Set (Set, singleton, delete, notMember)
import Data.Map (Map, lookup, insert, mapKeysMonotonic)
import qualified Data.Map as M
import Prelude hiding (lookup)
import Data.Foldable
import Test.QuickCheck
import Data.String

-- A class for generating fresh names e.g.
-- if names are represented as text then we could increment the
-- name as:
--   "[name]" \mapsto "[name]'"
class Increment a where
  incr :: a -> a

instance (IsString t, Semigroup t) => Increment t where
  incr = (<> fromString "'")

newtype NumIncr a = NumIncr {getNumIncr :: a}
  deriving newtype Num

instance  Num a => Increment (NumIncr a) where
  incr = (+ 1)

data Term a =
    Var a
  | Lam a (Term a)
  | App (Term a) (Term a)
  deriving (Eq, Show)

infixl 5 .$
(.$) :: Term a -> Term a -> Term a
(.$) = App

instance IsString t => IsString (Term t) where
  fromString = Var . fromString

instance Arbitrary a => Arbitrary (Term a) where
  arbitrary = sized $ \n -> genTerm n []
    where
      genTerm :: Int -> [a] -> Gen (Term a)
      genTerm size scope
        | size <= 0 = genVar scope
        | otherwise = frequency
            [ (3, genVar scope)
            , (2, genLam size scope)
            , (1, genApp size scope)
            ]

      genVar :: [a] -> Gen (Term a)
      genVar [] = Var <$> arbitrary  -- No vars in scope, generate arbitrary
      genVar scope = oneof
        [ Var <$> elements scope     -- Pick from in-scope vars
        , Var <$> arbitrary           -- Or generate a free var
        ]

      genLam :: Int -> [a] -> Gen (Term a)
      genLam size scope = do
        binder <- arbitrary
        body <- genTerm (size `div` 2) (binder : scope)
        pure $ Lam binder body

      genApp :: Int -> [a] -> Gen (Term a)
      genApp size scope = do
        let size' = size `div` 2
        App <$> genTerm size' scope <*> genTerm size' scope


data Var a = B Int | F a
  deriving (Eq, Show)

type Scope f x = f x


freeVars :: (Ord a) => Term a -> Set a
freeVars (Var a)          = singleton a
freeVars (App l r)        = freeVars l <> freeVars r
freeVars (Lam nm expr) = delete nm (freeVars expr)

allVars :: (Ord a) => Term a -> Set a
allVars (Var a)       = singleton a
allVars (App l r)     = allVars l <> allVars r
allVars (Lam nm expr) = singleton nm <> allVars expr

freeVarsLN :: (Ord a) => Term (Var a) -> Set a
freeVarsLN (Var v) = case v of
  F a  -> singleton a
  B _  -> mempty
freeVarsLN (App l r) = freeVarsLN l <> freeVarsLN r
freeVarsLN (Lam _ e) = freeVarsLN e

freshName :: (Ord a, Increment a, Semigroup a) => a -> Set a -> a
freshName candidate conflicts
  | candidate `notMember` conflicts = candidate
  | otherwise = freshName (incr candidate) conflicts

toLocallyNameless :: forall a . (Ord a) => Term a -> Term (Var a)
toLocallyNameless = go mempty
  where
    go :: Map a Int -> Term a -> Term (Var a)
    go env = \case
      Var a  ->
        case a `lookup` env of
          Just bv -> Var (B bv)
          Nothing -> Var (F a)
      App l r -> App (go env l) (go env r)
      Lam n e   ->
        let
          env' = insert n 0 (M.map (+ 1) env)
        in
          Lam (F n) (go env' e)


fromLocallyNameless :: forall a . (Ord a, Increment a, Semigroup a) => Term (Var a) -> Term a
fromLocallyNameless = go mempty
  where
    go :: Map Int a -> Term (Var a) -> Term a
    go env = \case
      Var v ->
        case v of
          F a  -> Var a
          B bv -> case bv `lookup` env of
            Just name -> Var name
            Nothing   -> error $ "Found bound variable :" <> show bv <> " without binder."
      App l r -> App (go env l) (go env r)
      Lam n e ->
        case n of
          B bv -> error $ "Found unnamed variable at binding site :" <> show bv
          F v  ->
            let
              -- when converting from locally nameless we need
              -- to choose a fresh name from the free variables
              -- in the body of the expression and then increment
              -- all bound variables in the environment
              fvs = freeVarsLN e
              v' = freshName v fvs
              env' = insert 0 v' (mapKeysMonotonic (+ 1) env)
            in
              Lam v' (go env' e)

alphaEq :: forall a . (Ord a) => Term a -> Term a -> Bool
alphaEq l r = toLocallyNameless l == toLocallyNameless r

(.==) :: forall a . (Ord a) => Term a -> Term a -> Bool
(.==) = alphaEq

-- |
-- Shift adjusts bound variable indices by a given amount
-- Only shifts variables >= cutoff (i.e., those that refer to outer binders)
-- Used when moving a term to a different binding depth
shift :: forall a . Int -> Term (Var a) -> Term (Var a)
shift amount = shiftFrom 0
  where
    shiftFrom :: Int -> Term (Var a) -> Term (Var a)
    shiftFrom cutoff = \case
      Var v -> case v of
        B bv | bv >= cutoff -> Var (B (bv + amount))
             | otherwise -> Var (B bv)
        F fv -> Var (F fv)
      App l r -> App (shiftFrom cutoff l) (shiftFrom cutoff r)
      Lam n b -> Lam n (shiftFrom (cutoff + 1) b)

-- |
-- Open takes a term with an outer binder and instantiates that binder
-- with a given term. If this term is a variable then this is the usual
-- open operator.
open :: forall a . Term (Var a) -> Scope Term (Var a) -> Term (Var a)
open image = go 0
  where
    go :: Int -> Term (Var a) -> Term (Var a)
    go outer =
      \case
        Var fbv ->
          case fbv of
            B bv | bv == outer -> shift outer image
                 | bv > outer  -> Var (B (bv - 1))
                 | otherwise   -> Var (B bv)
            F fv -> Var (F fv)
        App l r -> App (go outer l) (go outer r)
        Lam n b -> Lam n (go (outer + 1) b)

-- |
-- Close takes a term and a given free variable and converts that to an
-- outer binder. This can be uses to abstract a variable.
close :: forall a . (Eq a) => a -> Term (Var a) -> Scope Term (Var a)
close name = go 0
  where
    go :: Int -> Term (Var a) -> Scope Term (Var a)
    go outer =
      \case
        Var v ->
          case v of
            B bv -> Var (B bv)
            F fv | fv == name -> Var (B outer)
                 | otherwise  -> Var (F fv)
        App l r -> App (go outer l) (go outer r)
        Lam n b -> Lam n (go (outer + 1) b)



-- |
-- substitute is just a convenient short-hand for open.
substitute :: Term (Var a) -> Scope Term (Var a) -> Term (Var a)
substitute = open


whnfLN :: Show a => Term (Var a) -> Term (Var a)
whnfLN term = go 0 term []
  where
    maxSteps = 10000
    go :: Show a => Int -> Term (Var a) -> [Term (Var a)] -> Term (Var a)
    go steps t as
      | steps > maxSteps = error $ "whnfLN exceeded " ++ show maxSteps ++ " steps. Term: " ++ show t ++ ", Args: " ++ show (length as)
      | otherwise = case (t, as) of
          (App l r, args)
            -> go (steps + 1) l (r : args )
          (Lam _ body , a:args)
            -> go (steps + 1) (substitute a body) args
          _
            -> foldl' App t as


whnf :: (Ord a, Increment a, Semigroup a, Show a) => Term a -> Term a
whnf = fromLocallyNameless . whnfLN . toLocallyNameless


-- |
-- Reduce to normal form using a leftmost-outermost strategy.
nfLN :: Show a => Term (Var a) -> Term (Var a)
nfLN term = go 0 term []
  where
    maxSteps = 10000
    go :: Show a => Int -> Term (Var a) -> [Term (Var a)] -> Term (Var a)
    go steps t as
      | steps > maxSteps = error $ "nfLN exceeded " ++ show maxSteps ++ " steps. Term: " ++ show t ++ ", Args: " ++ show (length as)
      | otherwise = case (t, as) of
          (App l r, args)
            -> go (steps + 1) l (r : args)
          (Lam n body , [])
            -> Lam n (nfLN body)
          (Lam _ body , a:args)
            -> go (steps + 1) (substitute a body) args
          _
            -> foldl' App t (fmap nfLN as)


nf :: (Ord a, Increment a, Semigroup a, Show a) => Term a -> Term a
nf = fromLocallyNameless . nfLN . toLocallyNameless
