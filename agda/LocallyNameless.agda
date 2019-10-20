module LocallyNameless where

open import Data.Nat.Base using (ℕ; zero; suc)
open import Data.Fin using (Fin; inject₁; pred; fromℕ) renaming (zero to fzero; suc to fsuc)
open import Data.String using (String; _≟_)
open import Data.Bool using (Bool; true; false)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.Unit using (⊤; tt)
open import Relation.Nullary using (yes; no)

data LC_at (k : ℕ) : Set where
  fvar : String -> LC_at k
  bvar : Fin (suc k) -> LC_at k
  app  : LC_at k -> LC_at k -> LC_at k
  abs  : LC_at (suc k) -> LC_at k

LocallyClosed : Set
LocallyClosed = LC_at zero

inc : ∀ {k : ℕ} -> LC_at k -> LC_at (suc k)
inc (fvar x) = fvar x
inc (bvar x) = bvar (inject₁ x)
inc (app l r) = app (inc l) (inc r)
inc (abs t) = abs (inc t)


Body : Set
Body = LC_at 1


split : ∀ {n : ℕ} -> Fin (suc n) -> ⊤ ⊎ Fin n
split {zero} fn = inj₁ tt
split {suc zero} fzero = inj₂ fzero
split {suc zero} (fsuc fn) = inj₁ tt
split {suc (suc n)} fzero = inj₂ fzero
split {suc (suc n)} (fsuc fn) with split {suc n} fn 
split {suc (suc n)} (fsuc fn) | inj₁ tt = inj₁ tt
split {suc (suc n)} (fsuc fn) | inj₂ fn' = inj₂ (fsuc fn')


_^_ : ∀ {k : ℕ} -> LC_at (suc k) -> LC_at k -> LC_at k
fvar v ^ t = fvar v
bvar i ^ t with split i 
(bvar i ^ t) | inj₁ top = t
(bvar i ^ t) | inj₂ n = bvar n
app l r ^ t = app (l ^ t) (r ^ t)
abs body ^ t = abs (body ^ inc t)


_v_ : ∀ {k : ℕ} -> String -> LC_at k -> LC_at (suc k)
_v_ {k} var (fvar x) with var ≟ x
(_v_ {k} var (fvar x)) | yes p = bvar (fromℕ (suc k))
(var v fvar x) | no ¬p = fvar x
var v bvar x = {!!}
var v app term term₁ = {!!}
var v abs term = {!!}
