{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LN.Expression where

import Data.Set (Set, singleton, delete)
import Data.Map (Map, lookup, insert, mapKeysMonotonic)
import qualified Data.Map as M
import Prelude hiding (lookup)
import Data.Foldable
import Test.QuickCheck
import Data.String


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
  arbitrary =
    do
      i <- choose (0,10)
      buildTerm i
    where
      buildTerm :: Int -> Gen (Term a)
      buildTerm i
        | i <= 2    = arbitrary >>= pure . Var
        | i <= 8    = Lam <$> arbitrary <*> arbitrary
        | otherwise = App <$> arbitrary <*> arbitrary
 

data Var a = B Int | F a
  deriving (Eq)

type Scope f x = f x


freeVars :: (Ord a) => Term a -> Set a
freeVars (Var a)          = singleton a
freeVars (App l r)        = freeVars l <> freeVars r
freeVars (Lam nm expr) = delete nm (freeVars expr)


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


fromLocallyNameless :: forall a . (Ord a) => Term (Var a) -> Term a
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
              env' = insert 0 v (mapKeysMonotonic (+ 1) env)
            in
              Lam v (go env' e)

alphaEq :: forall a . (Ord a) => Term a -> Term a -> Bool
alphaEq l r = toLocallyNameless l == toLocallyNameless r

(.==) :: forall a . (Ord a) => Term a -> Term a -> Bool
(.==) = alphaEq

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
            B bv | bv == outer -> image
                 | otherwise -> Var (B bv)
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


whnfLN :: Term (Var a) -> Term (Var a)
whnfLN term = go term []
  where
    go :: Term (Var a) -> [Term (Var a)] -> Term (Var a)
    go t as =
      case (t, as) of
        (App l r, args)
          -> go l (r : args )
        (Lam _ body , a:args)
          -> go (substitute a body) args
        _
          -> foldl' App t as


whnf :: (Ord a) => Term a -> Term a
whnf = fromLocallyNameless . whnfLN . toLocallyNameless


-- |
-- Reduce to normal form using a [WHAT] strategy.
nfLN :: Term (Var a) -> Term (Var a)
nfLN term = go term []
  where
    go :: Term (Var a) -> [Term (Var a)] -> Term (Var a)
    go t as =
      case (t, as) of
        (App l r, args)
          -> go l (r : args)
        (Lam n body , [])
          -> Lam n (nfLN body)
        (Lam _ body , a:args)
          -> go (substitute a body) args
        _
          -> foldl' App t (fmap nfLN as)


nf :: (Ord a) => Term a -> Term a
nf = fromLocallyNameless . nfLN . toLocallyNameless


         
          
    
  
        
        
         
        
      
      

      
      


