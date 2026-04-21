{-# OPTIONS --safe #-}

-- This module defines a big-step parallel semantics for pointwise kernels
-- and proves it equals sequential execution for OutputInputConsistent kernels.
--
-- MOTIVATION: The rest of the theory uses sequential semantics (runWithSchedule),
-- but the paper's central claim is about *parallel* correctness.  This module
-- closes that gap by defining runParallel — in which all steps fire simultaneously,
-- reading only the original environment — and proving:
--
--   parallel-eq-sequential :
--     OutputInputConsistent base →
--     PostStateEq (runParallel env base vs) (runWithSchedule env base vs)
--
-- The proof is immediate from the existing infrastructure:
--   • At the output array:  both sides equal the canonical value
--     (schedule-canonical-output); runParallel computes this by definition.
--   • At every other array: both sides return the original environment
--     (runWhole-unrelated for the sequential side; runParallel by definition).
--
-- COROLLARY: parallel-schedule-invariance
--   Two parallel executions with different valid schedules for the same
--   OutputInputConsistent kernel produce identical post-states.
--
-- This is the formal justification for parFor: any two permutations of the
-- iteration domain fire the same writes and read only from the pre-execution
-- state, so parallel execution is semantically equivalent to sequential.

module Loom.Theory.ParallelSemantics where

open import Loom.Theory.Index
open import Loom.Theory.Pointwise
open import Loom.Theory.Prelude
open import Loom.Theory.ScheduleEquivalence
  using (schedule-canonical-output; schedule-equivalence)
open import Loom.Theory.ScheduleIndependent
open import Loom.Theory.Schedule
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.Traversal using (stepAt)
open import Loom.Theory.WholeLinear using (runWhole-unrelated)
open import Relation.Nullary using (no; yes)

-- Big-step parallel execution.
-- Each output cell is computed by applying transform to the value read from
-- the *original* environment env (not any intermediate state).  Non-output
-- arrays are left unchanged.
--
-- This models parFor: all iterations run simultaneously, reading the
-- pre-execution snapshot.
runParallel :
  ∀ {rank sched dom n} →
  Env rank →
  (base : PointwiseKernel {rank} sched dom) →
  ValidSchedule base n →
  Env rank
runParallel env base vs arr ix
  with arrayEq arr (outputArr base)
... | no  _    = env arr ix
... | yes refl =
    transform base
      (lookupEnv env (inputArr base)
        (resolve (inputAt base (stepAt (steps vs) (coverIndex vs ix)))))

-- HEADLINE THEOREM: parallel-eq-sequential.
--
-- For an OutputInputConsistent kernel, big-step parallel execution produces
-- the same post-state as sequential execution (runWithSchedule).
--
-- Proof sketch:
--   At the output array: runWithSchedule writes the canonical value at each
--   output cell (schedule-canonical-output).  runParallel writes exactly that
--   same value by definition.
--   At every other array: runWithSchedule leaves unrelated arrays unchanged
--   (runWhole-unrelated).  runParallel also returns env unchanged.
parallel-eq-sequential :
  ∀ {rank sched dom n} →
  (base : PointwiseKernel {rank} sched dom) →
  (vs : ValidSchedule base n) →
  (env : Env rank) →
  PostStateEq
    (runParallel env base vs)
    (runWithSchedule env base vs)
parallel-eq-sequential base vs env arr ix
  with arrayEq arr (outputArr base)
-- Case: unrelated array.  Both sides equal env arr ix.
parallel-eq-sequential base vs env arr ix | no arr≢output =
  sym (runWhole-unrelated (toWholeKernel base vs) env arr (λ eq → arr≢output (sym eq)) ix)
-- Case: output array.  runParallel writes the canonical value by definition;
-- runWithSchedule writes it by schedule-canonical-output (sym).
parallel-eq-sequential base vs env arr ix | yes refl =
  sym (schedule-canonical-output base vs env ix)

-- COROLLARY: parallel-schedule-invariance.
--
-- Any two valid schedules for the same OutputInputConsistent kernel produce
-- identical post-states when run in parallel.
--
-- Proof: chain through the common sequential result.
parallel-schedule-invariance :
  ∀ {rank sched dom n m} →
  (base : PointwiseKernel {rank} sched dom) →
  OutputInputConsistent base →
  (vs1 : ValidSchedule base n) →
  (vs2 : ValidSchedule base m) →
  (env : Env rank) →
  PostStateEq
    (runParallel env base vs1)
    (runParallel env base vs2)
parallel-schedule-invariance base oi vs1 vs2 env arr ix =
  trans
    (parallel-eq-sequential base vs1 env arr ix)
    (trans
      (schedule-equivalence base oi vs1 vs2 env arr ix)
      (sym (parallel-eq-sequential base vs2 env arr ix)))
