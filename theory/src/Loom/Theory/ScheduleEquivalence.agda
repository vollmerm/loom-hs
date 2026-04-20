{-# OPTIONS --safe #-}

-- This module proves the central schedule-equivalence theorem: for any kernel
-- satisfying OutputInputConsistent, all valid schedules produce the same output.
--
-- The proof has three steps:
--
--   1. schedule-canonical:
--        Any valid schedule produces output equal to its own expectedOutput
--        (the schedule-relative pointwise spec).  This is a direct consequence
--        of obs-correct / runWhole-state-eq.
--
--   2. schedule-independent-spec:
--        For an OutputInputConsistent kernel, the expectedOutput value at any
--        output cell is the same for every valid schedule.  The proof is a
--        single application of outputInputConsistent to the two cover steps.
--
--   3. schedule-equivalence (headline theorem):
--        Any two valid schedules for an OutputInputConsistent kernel produce
--        identical post-states.  Proof: three-step transitivity through the
--        common canonical value.
--
-- This is the Loom-theory formalization of the polyhedral model's core claim:
-- all legal schedules of a program are semantically equivalent.

module Loom.Theory.ScheduleEquivalence where

open import Loom.Theory.ExactCoverLinear
  using (runWhole-state-eq; runWhole-covered-pointwise; runWhole-state)
open import Loom.Theory.Index
open import Loom.Theory.Pointwise
open import Loom.Theory.Prelude
open import Loom.Theory.ScheduleIndependent
open import Loom.Theory.Schedule
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.Traversal using (stepAt)
open import Loom.Theory.WholeLinear using (runWhole; runWhole-unrelated)
open import Relation.Nullary using (no; yes)

-- Step 1: schedule-canonical (at output cells)
-- Any valid schedule writes to each output cell the value
--   transform(input[resolve(inputAt(step(coverIndex ix)))])
-- This follows directly from runWhole-covered-pointwise.
schedule-canonical-output :
  ∀ {rank sched dom n} →
  (base : PointwiseKernel {rank} sched dom) →
  (vs : ValidSchedule base n) →
  (env : Env rank) →
  (ix : RectIx (shape (outputArr base))) →
  lookupEnv (runWithSchedule env base vs) (outputArr base) ix ≡
    transform base
      (lookupEnv env (inputArr base)
        (resolve (inputAt base (stepAt (steps vs) (coverIndex vs ix)))))
schedule-canonical-output base vs env ix =
  runWhole-covered-pointwise (toExactCoverKernel base vs) env ix

-- Step 2: schedule-independent-spec
-- For an OutputInputConsistent kernel, the value written to each output cell
-- is the same under any two valid schedules.
--
-- Proof: both valid schedules have a unique step that writes to ix.
-- outputCovered tells us both steps land on ix.
-- OutputInputConsistent then forces both steps to read from the same input cell.
schedule-independent-spec :
  ∀ {rank sched dom n m} →
  (base : PointwiseKernel {rank} sched dom) →
  OutputInputConsistent base →
  (vs1 : ValidSchedule base n) →
  (vs2 : ValidSchedule base m) →
  (env : Env rank) →
  (ix : RectIx (shape (outputArr base))) →
  transform base
    (lookupEnv env (inputArr base)
      (resolve (inputAt base (stepAt (steps vs1) (coverIndex vs1 ix)))))
  ≡
  transform base
    (lookupEnv env (inputArr base)
      (resolve (inputAt base (stepAt (steps vs2) (coverIndex vs2 ix)))))
schedule-independent-spec base oi vs1 vs2 env ix =
  cong (transform base)
    (cong (lookupEnv env (inputArr base))
      (oi
        (stepAt (steps vs1) (coverIndex vs1 ix))
        (stepAt (steps vs2) (coverIndex vs2 ix))
        (trans
          (outputCovered vs1 ix)
          (sym (outputCovered vs2 ix)))))

-- Step 3: schedule-equivalence (headline theorem)
-- Any two valid schedules for an OutputInputConsistent kernel produce
-- identical post-states.
schedule-equivalence :
  ∀ {rank sched dom n m} →
  (base : PointwiseKernel {rank} sched dom) →
  OutputInputConsistent base →
  (vs1 : ValidSchedule base n) →
  (vs2 : ValidSchedule base m) →
  (env : Env rank) →
  PostStateEq
    (runWithSchedule env base vs1)
    (runWithSchedule env base vs2)
schedule-equivalence base oi vs1 vs2 env arr ix
  with arrayEq arr (outputArr base)
schedule-equivalence base oi vs1 vs2 env arr ix | yes refl =
  trans
    (schedule-canonical-output base vs1 env ix)
    (trans
      (schedule-independent-spec base oi vs1 vs2 env ix)
      (sym (schedule-canonical-output base vs2 env ix)))
schedule-equivalence base oi vs1 vs2 env arr ix | no arr≢output =
  trans
    (runWhole-unrelated (toWholeKernel base vs1) env arr (λ eq → arr≢output (sym eq)) ix)
    (sym (runWhole-unrelated (toWholeKernel base vs2) env arr (λ eq → arr≢output (sym eq)) ix))
