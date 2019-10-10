module LocallyNameless where

open import Data.Nat.Base using (ℕ; zero; suc)
open import Data.Fin using (Fin; inject₁; pred) renaming (zero to fzero; suc to fsuc)
open import Data.String using (String)
open import Data.Bool using (Bool; true; false)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.Unit using (⊤; tt)

data LC_at (k : ℕ) : Set where
  fvar : String -> LC_at k
  bvar : Fin (suc k) -> LC_at k
  app  : LC_at k -> LC_at k -> LC_at k
  abs  : LC_at (suc k) -> LC_at k

LocallyClosed : Set
LocallyClosed = LC_at zero


Body : Set
Body = LC_at 1


_≐_ : ∀ {n : ℕ} -> Fin n -> Fin n -> Bool
fzero ≐ fzero = true
fzero ≐ fsuc m = false
fsuc n ≐ fzero = false
fsuc n ≐ fsuc m = n ≐ m
-- need a function that takes a Fin suc n and if it is
-- maximal returns Nothing and otherwise returns Fin n
isMaximum : ∀ {n : ℕ} -> Fin (suc n) -> Bool
isMaximum {zero} fzero = true
isMaximum {suc n} fzero = false
isMaximum {suc n} (fsuc k) = isMaximum {n} k

split : ∀ {n : ℕ} -> Fin (suc n) -> ⊤ ⊎ Fin n
split {zero} fn = inj₁ tt
split {suc zero} fzero = inj₂ fzero
split {suc zero} (fsuc fn) = inj₁ tt
split {suc (suc n)} fzero = inj₂ fzero
split {suc (suc n)} (fsuc fn) with split {suc n} fn 
split {suc (suc n)} (fsuc fn) | inj₁ tt = inj₁ tt
split {suc (suc n)} (fsuc fn) | inj₂ fn' = inj₂ (fsuc fn')


_^_ : ∀ {k : ℕ} -> LC_at (suc k) -> String -> LC_at k
fvar v ^ x = fvar v
bvar i ^ x with split i 
(bvar i ^ x) | inj₁ t = fvar x
(bvar i ^ x) | inj₂ n = bvar n
app l r ^ x = app (l ^ x) (r ^ x)
abs body ^ x = abs (body ^ x)
