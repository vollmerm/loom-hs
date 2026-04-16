{-# OPTIONS --safe #-}

module Loom.Theory.Reduction where

open import Loom.Theory.Prelude
open import Loom.Theory.Semantics
open import Loom.Theory.Shape

record Reducer : Set₁ where
  field
    Carrier : Set
    init : Carrier
    step : Carrier → ℕ → Carrier
    done : Carrier → ℕ

open Reducer public

foldReducerFrom :
  ∀ {n} →
  (reducer : Reducer) →
  Carrier reducer →
  (Fin n → ℕ) →
  Carrier reducer
foldReducerFrom {zero} reducer acc values = acc
foldReducerFrom {suc n} reducer acc values =
  foldReducerFrom
    reducer
    (step reducer acc (values fzero))
    (λ i → values (fsuc i))

foldReducerState :
  ∀ {n} →
  (reducer : Reducer) →
  (Fin n → ℕ) →
  Carrier reducer
foldReducerState reducer values = foldReducerFrom reducer (init reducer) values

foldReducer :
  ∀ {n} →
  (reducer : Reducer) →
  (Fin n → ℕ) →
  ℕ
foldReducer reducer values = done reducer (foldReducerState reducer values)

foldStore1D :
  ∀ {n} →
  (reducer : Reducer) →
  Store (shape1 n) →
  ℕ
foldStore1D reducer store = foldReducer reducer store

foldArray1D :
  (reducer : Reducer) →
  (env : Env rank1) →
  (arr : Array rank1) →
  ℕ
foldArray1D reducer env (array ident (shape1 n)) =
  foldStore1D {n} reducer (lookupEnv env (array ident (shape1 n)))

foldReducerFrom-step :
  ∀ {n} →
  (reducer : Reducer) →
  (acc : Carrier reducer) →
  (values : Fin (suc n) → ℕ) →
  foldReducerFrom reducer acc values ≡
    foldReducerFrom reducer (step reducer acc (values fzero)) (λ i → values (fsuc i))
foldReducerFrom-step reducer acc values = refl

foldReducer-step :
  ∀ {n} →
  (reducer : Reducer) →
  (values : Fin (suc n) → ℕ) →
  foldReducer reducer values ≡
    done reducer
      (foldReducerFrom reducer (step reducer (init reducer) (values fzero)) (λ i → values (fsuc i)))
foldReducer-step reducer values = refl

sumReducer : Reducer
sumReducer = record
  { Carrier = ℕ
  ; init = zero
  ; step = _+_
  ; done = λ acc → acc
  }
