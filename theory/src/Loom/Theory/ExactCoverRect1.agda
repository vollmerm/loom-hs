{-# OPTIONS --safe #-}

module Loom.Theory.ExactCoverRect1 where

open import Loom.Theory.Access
open import Loom.Theory.ExactCoverLinear
  using (ExactCoverKernel; expectedOutput; runWhole-covered-pointwise; runWhole-state; runWhole-state-eq)
  renaming (wholeKernel to ecWholeKernel; coverIndex to ecCoverIndex; outputCovered to ecOutputCovered)
open import Loom.Theory.Index
open import Loom.Theory.Pointwise
open import Loom.Theory.Prelude
open import Loom.Theory.RectExecution
open import Loom.Theory.Schedule
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.WholeRect1
open import Relation.Nullary using (no; yes)

record ExactCoverRect1Kernel (n : ℕ) : Set where
  field
    wholeKernel : WholeRect1Kernel n
    coverIndex :
      RectIx (shape (outputArr (base wholeKernel))) → Fin n
    outputCovered :
      (target : RectIx (shape (outputArr (base wholeKernel)))) →
      resolve (outputAt (base wholeKernel) (coverIndex target)) ≡ target

open ExactCoverRect1Kernel public

-- Convert to the generic ExactCoverLinear.ExactCoverKernel.
toExactCoverKernel :
  ∀ {n} →
  ExactCoverRect1Kernel n →
  ExactCoverKernel {rank1} {rect} {shape1 n} n
toExactCoverKernel kernel = record
  { wholeKernel  = toWholeKernel (wholeKernel kernel)
  ; coverIndex   = coverIndex kernel
  ; outputCovered = outputCovered kernel
  }

runRect1-covered-pointwise :
  ∀ {n} →
  (kernel : ExactCoverRect1Kernel n) →
  (env : Env rank1) →
  (target : RectIx (shape (outputArr (base (wholeKernel kernel))))) →
  lookupEnv (runRect1 env (kernelProgram (base (wholeKernel kernel))))
    (outputArr (base (wholeKernel kernel)))
    target ≡
    transform (base (wholeKernel kernel))
      (lookupEnv env (inputArr (base (wholeKernel kernel)))
        (resolve (inputAt (base (wholeKernel kernel)) (coverIndex kernel target))))
runRect1-covered-pointwise kernel env target =
  runWhole-covered-pointwise (toExactCoverKernel kernel) env target

rect1Expected :
  ∀ {n} →
  (kernel : ExactCoverRect1Kernel n) →
  (env : Env rank1) →
  (arr : Array rank1) →
  RectIx (shape arr) →
  ℕ
rect1Expected kernel = expectedOutput (toExactCoverKernel kernel)

runRect1-state :
  ∀ {n} →
  (kernel : ExactCoverRect1Kernel n) →
  (env : Env rank1) →
  (arr : Array rank1) →
  (ix : RectIx (shape arr)) →
  lookupEnv (runRect1 env (kernelProgram (base (wholeKernel kernel)))) arr ix ≡
    rect1Expected kernel env arr ix
runRect1-state kernel env arr ix =
  runWhole-state (toExactCoverKernel kernel) env arr ix

runRect1-state-eq :
  ∀ {n} →
  (kernel : ExactCoverRect1Kernel n) →
  (env : Env rank1) →
  PostStateEq
    (runRect1 env (kernelProgram (base (wholeKernel kernel))))
    (rect1Expected kernel env)
runRect1-state-eq kernel env arr ix = runRect1-state kernel env arr ix
