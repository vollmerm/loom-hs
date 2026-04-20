{-# OPTIONS --safe #-}

-- This module proves kernel independence: two kernels with disjoint output
-- arrays that do not read from each other's outputs can be run in either
-- order and produce the same combined environment.
--
-- This is the formal basis for parallel composition of independent `parFor`
-- regions: if two loop bodies operate on non-overlapping parts of the store
-- (disjoint outputs, no cross-reads), they are semantically interchangeable.
--
-- The proof uses only the existing runWhole-unrelated and
-- runWhole-covered-pointwise infrastructure — no new inductions.

module Loom.Theory.KernelIndependence where

open import Loom.Theory.ExactCoverLinear
  using (ExactCoverKernel; runWhole-covered-pointwise; coverIndex)
open import Loom.Theory.Index
open import Loom.Theory.Pointwise
open import Loom.Theory.Prelude
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.Traversal using (stepAt)
open import Loom.Theory.WholeLinear using (WholeKernel; runWhole; runWhole-unrelated; base; steps)
open import Loom.Theory.ExactCoverLinear using (wholeKernel)
open import Relation.Nullary using (no; yes)

-- HEADLINE THEOREM: kernel independence.
--
-- Given two well-formed kernels k1 and k2 over the same rank environment:
--
--   (disjoint)   outputArr k1 ≢ outputArr k2
--   (k2 safe)    outputArr k2 ≢ inputArr  k1   (k2's output is not k1's input)
--   (k1 safe)    outputArr k1 ≢ inputArr  k2   (k1's output is not k2's input)
--
-- then running k1 before k2 and running k2 before k1 produce the same
-- post-state at every (array, index) pair.
kernel-independence :
  ∀ {rank sched1 dom1 sched2 dom2 n m} →
  (k1 : ExactCoverKernel {rank} {sched1} {dom1} n) →
  (k2 : ExactCoverKernel {rank} {sched2} {dom2} m) →
  outputArr (base (wholeKernel k1)) ≢ outputArr (base (wholeKernel k2)) →
  outputArr (base (wholeKernel k2)) ≢ inputArr  (base (wholeKernel k1)) →
  outputArr (base (wholeKernel k1)) ≢ inputArr  (base (wholeKernel k2)) →
  (env : Env rank) →
  PostStateEq
    (runWhole (runWhole env (wholeKernel k1)) (wholeKernel k2))
    (runWhole (runWhole env (wholeKernel k2)) (wholeKernel k1))
kernel-independence k1 k2 out1≢out2 out2≢in1 out1≢in2 env arr ix
  with arrayEq arr (outputArr (base (wholeKernel k1)))
     | arrayEq arr (outputArr (base (wholeKernel k2)))

-- Case: arr = outputArr k1.
-- LHS: k2 doesn't touch out1 → value = what k1 wrote = transform(env[in1][ix1]).
-- RHS: k1 on (k2-first env) at out1 → transform((k2-first env)[in1][ix1])
--      = transform(env[in1][ix1])  since k2 doesn't touch in1.
kernel-independence k1 k2 out1≢out2 out2≢in1 out1≢in2 env arr ix
    | yes refl | _ =
  let wk1 = wholeKernel k1
      wk2 = wholeKernel k2
      ix1 = resolve (inputAt (base wk1)
                (stepAt (steps wk1) (coverIndex k1 ix)))
  in
  trans
    (runWhole-unrelated wk2 (runWhole env wk1)
       (outputArr (base wk1)) (λ eq → out1≢out2 (sym eq)) ix)
    (trans
      (runWhole-covered-pointwise k1 env ix)
      (trans
        (cong (transform (base wk1))
          (sym (runWhole-unrelated wk2 env (inputArr (base wk1)) out2≢in1 ix1)))
        (sym (runWhole-covered-pointwise k1 (runWhole env wk2) ix))))

-- Case: arr = outputArr k2 (and ≠ outputArr k1 from the previous case).
-- Symmetric: k1 doesn't touch out2; k1 doesn't touch in2.
kernel-independence k1 k2 out1≢out2 out2≢in1 out1≢in2 env arr ix
    | no arr≢out1 | yes refl =
  let wk1 = wholeKernel k1
      wk2 = wholeKernel k2
      ix2 = resolve (inputAt (base wk2)
                (stepAt (steps wk2) (coverIndex k2 ix)))
  in
  trans
    (runWhole-covered-pointwise k2 (runWhole env wk1) ix)
    (trans
      (cong (transform (base wk2))
        (runWhole-unrelated wk1 env (inputArr (base wk2)) out1≢in2 ix2))
      (trans
        (sym (runWhole-covered-pointwise k2 env ix))
        (sym (runWhole-unrelated wk1 (runWhole env wk2)
               (outputArr (base wk2)) (λ eq → out1≢out2 eq) ix))))

-- Case: arr is unrelated to both outputs.
-- Both sides reduce to lookupEnv env arr ix.
kernel-independence k1 k2 out1≢out2 out2≢in1 out1≢in2 env arr ix
    | no arr≢out1 | no arr≢out2 =
  let wk1 = wholeKernel k1
      wk2 = wholeKernel k2
  in
  trans
    (runWhole-unrelated wk2 (runWhole env wk1) arr (λ eq → arr≢out2 (sym eq)) ix)
    (trans
      (runWhole-unrelated wk1 env arr (λ eq → arr≢out1 (sym eq)) ix)
      (sym (trans
        (runWhole-unrelated wk1 (runWhole env wk2) arr (λ eq → arr≢out1 (sym eq)) ix)
        (runWhole-unrelated wk2 env arr (λ eq → arr≢out2 (sym eq)) ix))))

