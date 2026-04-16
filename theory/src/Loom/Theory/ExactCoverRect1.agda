{-# OPTIONS --safe #-}

module Loom.Theory.ExactCoverRect1 where

open import Loom.Theory.Access
open import Loom.Theory.Index
open import Loom.Theory.Pointwise
open import Loom.Theory.Prelude
open import Loom.Theory.RectExecution
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
  trans
    (cong
      (lookupEnv (runRect1 env (kernelProgram (base (wholeKernel kernel))))
        (outputArr (base (wholeKernel kernel))))
      (sym (outputCovered kernel target)))
    (runRect1-pointwise (wholeKernel kernel) env (coverIndex kernel target))

rect1Expected :
  ∀ {n} →
  (kernel : ExactCoverRect1Kernel n) →
  (env : Env rank1) →
  (arr : Array rank1) →
  RectIx (shape arr) →
  ℕ
rect1Expected kernel env arr ix with arrayEq arr (outputArr (base (wholeKernel kernel)))
... | yes refl =
  transform (base (wholeKernel kernel))
    (lookupEnv env (inputArr (base (wholeKernel kernel)))
      (resolve (inputAt (base (wholeKernel kernel)) (coverIndex kernel ix))))
... | no _ = lookupEnv env arr ix

runRect1-state :
  ∀ {n} →
  (kernel : ExactCoverRect1Kernel n) →
  (env : Env rank1) →
  (arr : Array rank1) →
  (ix : RectIx (shape arr)) →
  lookupEnv (runRect1 env (kernelProgram (base (wholeKernel kernel)))) arr ix ≡
    rect1Expected kernel env arr ix
runRect1-state kernel env arr ix with arrayEq arr (outputArr (base (wholeKernel kernel)))
... | yes refl = runRect1-covered-pointwise kernel env ix
... | no arr≢output =
  runRect1-unrelated
    (wholeKernel kernel)
    env
    arr
    (λ eq → arr≢output (sym eq))
    ix

runRect1-state-eq :
  ∀ {n} →
  (kernel : ExactCoverRect1Kernel n) →
  (env : Env rank1) →
  PostStateEq
    (runRect1 env (kernelProgram (base (wholeKernel kernel))))
    (rect1Expected kernel env)
runRect1-state-eq kernel env arr ix = runRect1-state kernel env arr ix
