{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module LN.Church where

import Data.Text
import LN.Expression

data Nat = Z | S Nat

fromInt :: Int -> Nat
fromInt 0 = Z
fromInt n = S (fromInt (n - 1))

fromNat :: Nat -> Term Text
fromNat Z = Lam "S" (Lam "Z" "Z")
fromNat (S n) = Lam "S" (Lam "Z" ("S" .$ (nf $ fromNat n .$ "S" .$ "Z")))

cZero  = fromNat Z
cOne   = fromNat (S Z)
cTwo   = fromNat (S (S Z))
cThree = fromNat (S (S (S Z)))
cFour  = fromNat (S (S (S (S Z))))
cFive  = fromNat (S (S (S (S (S Z)))))
cSix   = fromNat (S (S (S (S (S (S Z))))))


churchAdd :: Term Text
churchAdd = Lam "m" (Lam "n" (Lam "S" (Lam "Z" ((App "m" "S") .$ ("n" .$ "S" .$ "Z")))))


churchMult :: Term Text
churchMult = Lam "m" (Lam "n" (Lam "S" (Lam "Z" ("n" .$ ("m" .$ "S") .$ "Z"))))
