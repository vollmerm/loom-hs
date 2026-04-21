{-# OPTIONS --safe #-}

module Loom.Theory.ExactCoverLinear where

open import Loom.Theory.Index
open import Loom.Theory.Pointwise
open import Loom.Theory.Prelude
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.Traversal using (stepAt)
open import Loom.Theory.WholeLinear
open import Relation.Nullary using (no; yes)

-- A kernel whose whole-program run is fully characterized: every output index
-- is visited by exactly one step (witnessed by coverIndex + outputCovered).
record ExactCoverKernel
    {rank sched dom}
    (n : ℕ) : Set where
  field
    wholeKernel : WholeKernel {rank} {sched} {dom} n
    coverIndex  :
      RectIx (shape (outputArr (base wholeKernel))) → Fin n
    outputCovered :
      (target : RectIx (shape (outputArr (base wholeKernel)))) →
      resolve (outputAt (base wholeKernel)
        (stepAt (steps wholeKernel) (coverIndex target))) ≡ target

open ExactCoverKernel public

runWhole-covered-pointwise :
  ∀ {rank sched dom n} →
  (kernel : ExactCoverKernel {rank} {sched} {dom} n) →
  (env : Env rank) →
  (target : RectIx (shape (outputArr (base (wholeKernel kernel))))) →
  lookupEnv (runWhole env (wholeKernel kernel))
    (outputArr (base (wholeKernel kernel)))
    target ≡
    transform (base (wholeKernel kernel))
      (lookupEnv env (inputArr (base (wholeKernel kernel)))
        (resolve (inputAt (base (wholeKernel kernel))
          (stepAt (steps (wholeKernel kernel)) (coverIndex kernel target)))))
runWhole-covered-pointwise kernel env target =
  trans
    (cong
      (lookupEnv (runWhole env (wholeKernel kernel))
        (outputArr (base (wholeKernel kernel))))
      (sym (outputCovered kernel target)))
    (runWhole-pointwise (wholeKernel kernel) env (coverIndex kernel target))

expectedOutput :
  ∀ {rank sched dom n} →
  (kernel : ExactCoverKernel {rank} {sched} {dom} n) →
  (env : Env rank) →
  (arr : Array rank) →
  RectIx (shape arr) →
  ℕ
expectedOutput kernel env arr ix
  with arrayEq arr (outputArr (base (wholeKernel kernel)))
... | yes refl =
  transform (base (wholeKernel kernel))
    (lookupEnv env (inputArr (base (wholeKernel kernel)))
      (resolve (inputAt (base (wholeKernel kernel))
        (stepAt (steps (wholeKernel kernel)) (coverIndex kernel ix)))))
... | no _ = lookupEnv env arr ix

runWhole-state :
  ∀ {rank sched dom n} →
  (kernel : ExactCoverKernel {rank} {sched} {dom} n) →
  (env : Env rank) →
  (arr : Array rank) →
  (ix : RectIx (shape arr)) →
  lookupEnv (runWhole env (wholeKernel kernel)) arr ix ≡
    expectedOutput kernel env arr ix
runWhole-state kernel env arr ix
  with arrayEq arr (outputArr (base (wholeKernel kernel)))
... | yes refl = runWhole-covered-pointwise kernel env ix
... | no arr≢output =
  runWhole-unrelated
    (wholeKernel kernel)
    env
    arr
    (λ eq → arr≢output (sym eq))
    ix

runWhole-state-eq :
  ∀ {rank sched dom n} →
  (kernel : ExactCoverKernel {rank} {sched} {dom} n) →
  (env : Env rank) →
  PostStateEq
    (runWhole env (wholeKernel kernel))
    (expectedOutput kernel env)
runWhole-state-eq kernel env arr ix = runWhole-state kernel env arr ix
