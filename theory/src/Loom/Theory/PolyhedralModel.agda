{-# OPTIONS --safe #-}

-- This module provides the paper-facing statement of the polyhedral model
-- invariance theorem: all legal schedules of a pointwise kernel produce the
-- same output.
--
-- Terminology mapping to the polyhedral model:
--
--   POLYHEDRAL MODEL CONCEPT               LOOM AGDA CONCEPT
--   ─────────────────────────────          ──────────────────────────────────
--   No carried cross-iteration dependence  OutputInputConsistent
--   Legal execution order                  ValidSchedule (exact-cover witness)
--   Semantic invariance under scheduling   schedule-equivalence
--   Class of parallelizable programs       OutputInputConsistent pointwise kernels
--
-- The central claim is schedule-equivalence:
--
--   ∀ base, OutputInputConsistent base → ∀ vs1 vs2 : ValidSchedule base _,
--   PostStateEq (runWithSchedule env base vs1) (runWithSchedule env base vs2)
--
-- packaged here as polyhedral-schedule-invariance.

module Loom.Theory.PolyhedralModel where

open import Data.Fin.Base using (opposite)
open import Data.Fin.Properties using (opposite-involutive)
open import Loom.Theory.Examples
  using (line-copy-kernel; line-initial)
open import Loom.Theory.Index
open import Loom.Theory.Pointwise
open import Loom.Theory.Prelude
open import Loom.Theory.ScheduleEquivalence
  using (schedule-equivalence)
open import Loom.Theory.ScheduleIndependent
open import Loom.Theory.Semantics
open import Loom.Theory.Shape using (rank1)
open import Loom.Theory.Traversal using (linearTraversal)

-- A LegalSchedule packages a base kernel, its OutputInputConsistent witness
-- (the "no carried dependence" condition), and a valid exact-cover execution
-- order (the "legal schedule" of the polyhedral model).
record LegalSchedule
    {rank sc dom}
    (base : PointwiseKernel {rank} sc dom)
    (n : ℕ) : Set where
  field
    oi    : OutputInputConsistent base
    valid : ValidSchedule base n

open LegalSchedule public

-- HEADLINE THEOREM: polyhedral schedule invariance.
-- Any two legal schedules for the same base kernel produce identical post-states.
--
-- This is the Loom mechanization of the polyhedral model's central claim:
-- any two iteration orderings satisfying the exact-cover and no-carried-dependence
-- conditions are semantically equivalent.
polyhedral-schedule-invariance :
  ∀ {rank sc dom n m} →
  (base : PointwiseKernel {rank} sc dom) →
  (ls1 : LegalSchedule base n) →
  (ls2 : LegalSchedule base m) →
  (env : Env rank) →
  PostStateEq
    (runWithSchedule env base (valid ls1))
    (runWithSchedule env base (valid ls2))
polyhedral-schedule-invariance base ls1 ls2 env =
  schedule-equivalence base (oi ls1) (valid ls1) (valid ls2) env

-- CONCRETE EXAMPLE: forward vs backward ordering of the 1D line-copy kernel.
--
-- The line-copy kernel copies a 3-element array (identity transform).
-- We exhibit two valid schedules:
--   • forward: visits elements in order 0, 1, 2
--   • backward: visits elements in order 2, 1, 0  (using Data.Fin.Base.opposite)
--
-- polyhedral-schedule-invariance then confirms both produce the same post-state.

-- Forward schedule: elements visited in index order.
forward-schedule : ValidSchedule line-copy-kernel 3
forward-schedule = record
  { steps         = linearTraversal (λ i → i)
  ; outputUnique  = λ h → h
  ; coverIndex    = λ target → target
  ; outputCovered = λ _ → refl
  }

-- Backward schedule: elements visited in reverse index order.
-- outputUnique: if opposite i = opposite j then i = j, proved via involutivity.
-- outputCovered: opposite (opposite target) = target by opposite-involutive.
backward-schedule : ValidSchedule line-copy-kernel 3
backward-schedule = record
  { steps         = linearTraversal opposite
  ; outputUnique  = λ {i} {j} h →
      trans
        (sym (opposite-involutive i))
        (trans (cong opposite h) (opposite-involutive j))
  ; coverIndex    = opposite
  ; outputCovered = opposite-involutive
  }

-- Both schedules satisfy the LegalSchedule predicate.
forward-legal : LegalSchedule line-copy-kernel 3
forward-legal = record { oi = line-copy-oi-consistent ; valid = forward-schedule }

backward-legal : LegalSchedule line-copy-kernel 3
backward-legal = record { oi = line-copy-oi-consistent ; valid = backward-schedule }

-- COROLLARY: forward and backward orderings produce the same post-state.
forward-backward-equivalent :
  (env : Env rank1) →
  PostStateEq
    (runWithSchedule env line-copy-kernel forward-schedule)
    (runWithSchedule env line-copy-kernel backward-schedule)
forward-backward-equivalent env =
  polyhedral-schedule-invariance line-copy-kernel forward-legal backward-legal env
