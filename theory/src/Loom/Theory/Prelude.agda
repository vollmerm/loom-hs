{-# OPTIONS --safe #-}

module Loom.Theory.Prelude where

open import Data.Empty using (⊥; ⊥-elim) public
open import Data.Fin.Base using (Fin; fromℕ<; toℕ) renaming (zero to fzero; suc to fsuc) public
open import Data.Nat.Base using (ℕ; _+_; _<_; _≤_; zero; suc; z≤n; s≤s) public
open import Data.Product.Base using (_×_; _,_; proj₁; proj₂) public
open import Data.Unit.Base using (⊤; tt) public
open import Relation.Binary.PropositionalEquality using (_≡_; _≢_; refl; sym; trans; cong; subst) public
