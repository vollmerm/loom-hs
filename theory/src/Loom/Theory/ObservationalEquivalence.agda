{-# OPTIONS --safe #-}

-- This module provides the paper-facing statement of the main correctness theorem:
--
--   Any well-formed loop program is observationally equivalent to its
--   pointwise specification.
--
-- "Well-formed" means the program satisfies two disciplines:
--
--   ACCESS DISCIPLINE
--     • The input and output array handles are distinct.
--     • Every read is mediated by a capability-typed Access ReadOnly witness.
--     • Every write is mediated by a capability-typed Access WriteOnly witness.
--     • Each output cell is written at most once (outputUnique).
--     • There is a pure per-element function (transform) whose result is written.
--
--   COVERAGE DISCIPLINE
--     • Every output cell is covered: there is a coverIndex function mapping
--       each output coordinate to the step that writes it, together with a
--       proof (coverWitness) that the named step lands on that coordinate.
--
-- Together these guarantee exactly-once write discipline and yield a
-- deterministic, schedule-independent pointwise result.

module Loom.Theory.ObservationalEquivalence where

open import Loom.Theory.ExactCoverLinear
  using (ExactCoverKernel; expectedOutput;
         runWhole-state-eq; runWhole-covered-pointwise)
  renaming (wholeKernel to ecWholeKernel; coverIndex to coverIndex)
open import Loom.Theory.WholeLinear
  using (WholeKernel; runWhole; runWhole-unrelated; steps)
open import Loom.Theory.Pointwise
  using (PointwiseKernel; outputArr; inputArr; transform; inputAt)
open import Loom.Theory.WholeLinear
  using (base)
open import Loom.Theory.Semantics
  using (Env; lookupEnv; PostStateEq; resolve)
open import Loom.Theory.Shape
  using (Rank; Shape; Array; shape; rank1; shape1)
open import Loom.Theory.Schedule
  using (Schedule; rect)
open import Loom.Theory.Index
  using (Index; RectIx)
open import Loom.Theory.Prelude
open import Loom.Theory.Traversal
  using (LinearTraversal; stepAt)
open import Loom.Theory.Examples
  using (line-exact-kernel; line-initial; line-output; line-2)
open import Loom.Theory.ExactCoverRect1
  using (toExactCoverKernel)

-- A WellFormedKernel is an exact-cover kernel: it packages the access discipline
-- and the coverage discipline into a single record.  n is the number of loop steps.
WellFormedKernel :
  ∀ {rank : Rank} {sched : Schedule rank} {dom : Shape rank} →
  ℕ → Set
WellFormedKernel {rank} {sched} {dom} n =
  ExactCoverKernel {rank} {sched} {dom} n

-- The pointwise specification function.
-- For every cell of the output array:
--   pointwiseSpec k env outputArr ix = transform(env[inputArr][inputAt(step(coverIndex ix))])
-- For every other array:
--   pointwiseSpec k env arr ix = env[arr][ix]
pointwiseSpec :
  ∀ {rank sched dom n} →
  WellFormedKernel {rank} {sched} {dom} n →
  Env rank →
  (arr : Array rank) →
  RectIx (shape arr) →
  ℕ
pointwiseSpec = expectedOutput

-- Execute a well-formed kernel on an initial environment.
run :
  ∀ {rank sched dom n} →
  Env rank →
  WellFormedKernel {rank} {sched} {dom} n →
  Env rank
run env k = runWhole env (ecWholeKernel k)

-- MAIN THEOREM
-- Any well-formed kernel is observationally equivalent to its pointwise
-- specification.  Observational equivalence is pointwise equality of
-- lookupEnv at every (array, index) pair.
obs-correct :
  ∀ {rank sched dom n} →
  (k : WellFormedKernel {rank} {sched} {dom} n) →
  (env : Env rank) →
  PostStateEq (run env k) (pointwiseSpec k env)
obs-correct k env = runWhole-state-eq k env

-- COROLLARY: output cells
-- At every output coordinate ix, the result equals transform applied to the
-- input value at the corresponding input coordinate.
obs-correct-at :
  ∀ {rank sched dom n} →
  (k : WellFormedKernel {rank} {sched} {dom} n) →
  (env : Env rank) →
  (ix : RectIx (shape (outputArr (base (ecWholeKernel k))))) →
  lookupEnv (run env k) (outputArr (base (ecWholeKernel k))) ix ≡
    transform (base (ecWholeKernel k))
      (lookupEnv env (inputArr (base (ecWholeKernel k)))
        (resolve (inputAt (base (ecWholeKernel k))
          (stepAt (steps (ecWholeKernel k)) (coverIndex k ix)))))
obs-correct-at k env ix = runWhole-covered-pointwise k env ix

-- COROLLARY: unrelated arrays
-- Any array other than the output is unchanged by the kernel.
obs-correct-unrelated :
  ∀ {rank sched dom n} →
  (k : WellFormedKernel {rank} {sched} {dom} n) →
  (env : Env rank) →
  (other : Array rank) →
  outputArr (base (ecWholeKernel k)) ≢ other →
  (ix : RectIx (shape other)) →
  lookupEnv (run env k) other ix ≡ lookupEnv env other ix
obs-correct-unrelated k env other ≢other ix =
  runWhole-unrelated (ecWholeKernel k) env other ≢other ix

-- EXAMPLE: 1D line-copy kernel
-- The line-copy kernel (identity transform over a 3-element 1D array) is
-- well-formed; obs-correct applies directly.
line-copy-wfk : WellFormedKernel {rank1} {rect} {shape1 3} 3
line-copy-wfk = toExactCoverKernel line-exact-kernel

line-copy-obs-correct :
  PostStateEq (run line-initial line-copy-wfk) (pointwiseSpec line-copy-wfk line-initial)
line-copy-obs-correct = obs-correct line-copy-wfk line-initial

-- The output value at index 2 equals the input value (4).
line-copy-output-at-2 :
  lookupEnv (run line-initial line-copy-wfk) line-output line-2 ≡ 4
line-copy-output-at-2 = line-copy-obs-correct line-output line-2
