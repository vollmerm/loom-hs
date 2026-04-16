{-# OPTIONS --safe #-}

module Loom.Theory.Determinism where

open import Loom.Theory.Index
open import Loom.Theory.Prelude
open import Loom.Theory.SchedulePreservation
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.TiledPointwise
open import Loom.Theory.WholeTiled
open import Relation.Nullary using (no; yes)

schedule-preserves-state :
  ∀ {dom tileShape n m} →
  (kernel : TiledPointwiseKernel dom tileShape) →
  (eqv : EquivalentTraversals kernel n m) →
  (env : Env rank2) →
  PostStateEq
    (runTiles env (traversalWholeKernel kernel (left eqv)))
    (runTiles env (traversalWholeKernel kernel (right eqv)))
schedule-preserves-state kernel eqv env arr ix with arrayEq arr (outputArr kernel)
schedule-preserves-state kernel eqv env arr ix | yes refl =
  schedule-preserves-output kernel eqv env ix
schedule-preserves-state kernel eqv env arr ix | no arr≢output =
  trans
    (runTiles-unrelated
      (traversalWholeKernel kernel (left eqv))
      env
      arr
      (λ eq → arr≢output (sym eq))
      ix)
    (sym
      (runTiles-unrelated
        (traversalWholeKernel kernel (right eqv))
        env
        arr
        (λ eq → arr≢output (sym eq))
        ix))

disciplined-determinism :
  ∀ {dom tileShape n m} →
  (kernel : TiledPointwiseKernel dom tileShape) →
  (eqv : EquivalentTraversals kernel n m) →
  (env : Env rank2) →
  (arr : Array rank2) →
  (ix : RectIx (shape arr)) →
  lookupEnv (runTiles env (traversalWholeKernel kernel (left eqv))) arr ix ≡
    lookupEnv (runTiles env (traversalWholeKernel kernel (right eqv))) arr ix
disciplined-determinism kernel eqv env arr ix =
  schedule-preserves-state kernel eqv env arr ix
