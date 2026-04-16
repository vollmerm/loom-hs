{-# OPTIONS --safe #-}

module Loom.Theory.ReductionTheorems where

open import Data.Nat.Properties as ℕₚ using (+-assoc; +-identityʳ)
open import Loom.Theory.Access
open import Loom.Theory.ExactCoverRect1
open import Loom.Theory.Examples
open import Loom.Theory.Pointwise
open import Loom.Theory.Prelude
open import Loom.Theory.RectExecution
open import Loom.Theory.Reduction
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.WholeRect1

sumFin : ∀ {n} → (Fin n → ℕ) → ℕ
sumFin {zero} values = zero
sumFin {suc n} values = values fzero + sumFin (λ i → values (fsuc i))

sumFin-cong :
  ∀ {n} {left right : Fin n → ℕ} →
  ((i : Fin n) → left i ≡ right i) →
  sumFin left ≡ sumFin right
sumFin-cong {zero} eq = refl
sumFin-cong {suc n} {left} {right} eq
  rewrite eq fzero =
    cong
      (λ rest → right fzero + rest)
      (sumFin-cong (λ i → eq (fsuc i)))

foldReducerFrom-sum :
  ∀ {n} →
  (acc : ℕ) →
  (values : Fin n → ℕ) →
  foldReducerFrom sumReducer acc values ≡ acc + sumFin values
foldReducerFrom-sum {zero} acc values = sym (ℕₚ.+-identityʳ acc)
foldReducerFrom-sum {suc n} acc values =
  trans
    (foldReducerFrom-sum (acc + values fzero) (λ i → values (fsuc i)))
    (ℕₚ.+-assoc acc (values fzero) (sumFin (λ i → values (fsuc i))))

foldReducer-sum-correct :
  ∀ {n} →
  (values : Fin n → ℕ) →
  foldReducer sumReducer values ≡ sumFin values
foldReducer-sum-correct values = foldReducerFrom-sum zero values

foldStore1D-sum-correct :
  ∀ {n} →
  (store : Store (shape1 n)) →
  foldStore1D sumReducer store ≡ sumFin store
foldStore1D-sum-correct store = foldReducer-sum-correct store

foldArray1D-sum-correct :
  (env : Env rank1) →
  (ident : ℕ) →
  (n : ℕ) →
  foldArray1D sumReducer env (array ident (shape1 n)) ≡
    sumFin (lookupEnv env (array ident (shape1 n)))
foldArray1D-sum-correct env ident n =
  foldStore1D-sum-correct (lookupEnv env (array ident (shape1 n)))

line-sum-correct :
  foldArray1D sumReducer line-initial line-input ≡ sumFin (lookupEnv line-initial line-input)
line-sum-correct =
  foldArray1D-sum-correct line-initial (ident line-input) (extent1 (shape line-input))

line-sum-is-12 :
  foldArray1D sumReducer line-initial line-input ≡ 12
line-sum-is-12 = trans line-sum-correct refl

line-inc-sum-correct :
  foldArray1D sumReducer (runRect1 line-initial line-rect-inc) line-inc-output ≡
    foldArray1D sumReducer (rect1Expected line-inc-exact-kernel line-initial) line-inc-output
line-inc-sum-correct =
  trans
    (foldArray1D-sum-correct
      (runRect1 line-initial line-rect-inc)
      (ident line-inc-output)
      (extent1 (shape line-inc-output))
    )
    (trans
      (sumFin-cong
        (λ ix →
          runRect1-state
            line-inc-exact-kernel
            line-initial
            line-inc-output
            ix))
      (sym
        (foldArray1D-sum-correct
          (rect1Expected line-inc-exact-kernel line-initial)
          (ident line-inc-output)
          (extent1 (shape line-inc-output))
        )))

line-inc-sum-is-15 :
  foldArray1D sumReducer (runRect1 line-initial line-rect-inc) line-inc-output ≡ 15
line-inc-sum-is-15 =
  trans
    line-inc-sum-correct
    refl
