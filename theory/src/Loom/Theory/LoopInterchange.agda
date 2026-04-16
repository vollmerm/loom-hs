{-# OPTIONS --safe #-}

module Loom.Theory.LoopInterchange where

open import Loom.Theory.Access
open import Loom.Theory.ExactCoverRect2
open import Loom.Theory.Index
open import Loom.Theory.Pointwise
open import Loom.Theory.Prelude
open import Loom.Theory.RectExecution
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.WholeRect2
open import Relation.Nullary using (no; yes)

runCol-step :
  ∀ {rows cols} →
  (kernel : WholeRect2Kernel (suc rows) cols) →
  (env : Env rank2) →
  (col : Fin cols) →
  runCol env (kernelProgram (base kernel)) col ≡
    runCol
      (runAt env (kernelProgram (base kernel)) (fzero , col))
      (kernelProgram (base (tailRows kernel)))
      col
runCol-step kernel env col = refl

runCol-unrelated :
  ∀ {rows cols} →
  (kernel : WholeRect2Kernel rows cols) →
  (env : Env rank2) →
  (col : Fin cols) →
  (other : Array rank2) →
  outputArr (base kernel) ≢ other →
  (j : RectIx (shape other)) →
  lookupEnv (runCol env (kernelProgram (base kernel)) col) other j ≡ lookupEnv env other j
runCol-unrelated {zero} kernel env col other output≢other j = refl
runCol-unrelated {suc rows} kernel env col other output≢other j
  rewrite runCol-step kernel env col =
    trans
      (runCol-unrelated
        (tailRows kernel)
        (runAt env (kernelProgram (base kernel)) (fzero , col))
        col
        other
        output≢other
        j)
      (runAt-unrelated (base kernel) env (fzero , col) other output≢other j)

runCol-preserve-target :
  ∀ {rows cols} →
  (kernel : WholeRect2Kernel rows cols) →
  (env : Env rank2) →
  (col : Fin cols) →
  (target : RectIx (shape (outputArr (base kernel)))) →
  (noHit : ∀ row → resolve (outputAt (base kernel) (row , col)) ≢ target) →
  lookupEnv (runCol env (kernelProgram (base kernel)) col) (outputArr (base kernel)) target ≡
    lookupEnv env (outputArr (base kernel)) target
runCol-preserve-target {zero} kernel env col target noHit = refl
runCol-preserve-target {suc rows} kernel env col target noHit
  rewrite runCol-step kernel env col =
    trans
      (runCol-preserve-target
        (tailRows kernel)
        (runAt env (kernelProgram (base kernel)) (fzero , col))
        col
        target
        (λ row → noHit (fsuc row)))
      (updateEnv-other-index
        env
        (outputArr (base kernel))
        (outputAt (base kernel) (fzero , col))
        (transform (base kernel)
          (lookupEnv env (inputArr (base kernel))
            (resolve (inputAt (base kernel) (fzero , col)))))
        target
        (noHit fzero))

runCol-pointwise :
  ∀ {rows cols} →
  (kernel : WholeRect2Kernel rows cols) →
  (env : Env rank2) →
  (row : Fin rows) →
  (col : Fin cols) →
  lookupEnv (runCol env (kernelProgram (base kernel)) col) (outputArr (base kernel))
    (resolve (outputAt (base kernel) (row , col))) ≡
    transform (base kernel)
      (lookupEnv env (inputArr (base kernel)) (resolve (inputAt (base kernel) (row , col))))
runCol-pointwise {zero} kernel env () col
runCol-pointwise {suc rows} kernel env fzero col
  rewrite runCol-step kernel env col =
    trans
      (runCol-preserve-target
        (tailRows kernel)
        (runAt env (kernelProgram (base kernel)) (fzero , col))
        col
        (resolve (outputAt (base kernel) (fzero , col)))
        (λ row eq → fsuc≢fzero (cong proj₁ (outputUnique kernel eq))))
      (runAt-pointwise (base kernel) env (fzero , col))
runCol-pointwise {suc rows} kernel env (fsuc row) col
  rewrite runCol-step kernel env col =
    trans
      (runCol-pointwise
        (tailRows kernel)
        (runAt env (kernelProgram (base kernel)) (fzero , col))
        row
        col)
      (cong
        (transform (base kernel))
        (runAt-input-preserved
          (base kernel)
          env
          (fzero , col)
          (resolve (inputAt (base kernel) (fsuc row , col)))))

runCol-input-preserved :
  ∀ {rows cols} →
  (kernel : WholeRect2Kernel rows cols) →
  (env : Env rank2) →
  (col : Fin cols) →
  (j : RectIx (shape (inputArr (base kernel)))) →
  lookupEnv (runCol env (kernelProgram (base kernel)) col) (inputArr (base kernel)) j ≡
    lookupEnv env (inputArr (base kernel)) j
runCol-input-preserved kernel env col j =
  runCol-unrelated kernel env col (inputArr (base kernel)) (λ eq → distinct (base kernel) (sym eq)) j

runRect2Interchange-step :
  ∀ {rows cols} →
  (kernel : WholeRect2Kernel rows (suc cols)) →
  (env : Env rank2) →
  runRect2Interchange env (kernelProgram (base kernel)) ≡
    runRect2Interchange
      (runCol env (kernelProgram (base kernel)) fzero)
      (kernelProgram (base (tailCols kernel)))
runRect2Interchange-step kernel env = refl

runRect2Interchange-unrelated :
  ∀ {rows cols} →
  (kernel : WholeRect2Kernel rows cols) →
  (env : Env rank2) →
  (other : Array rank2) →
  outputArr (base kernel) ≢ other →
  (j : RectIx (shape other)) →
  lookupEnv (runRect2Interchange env (kernelProgram (base kernel))) other j ≡ lookupEnv env other j
runRect2Interchange-unrelated {rows} {zero} kernel env other output≢other j = refl
runRect2Interchange-unrelated {rows} {suc cols} kernel env other output≢other j
  rewrite runRect2Interchange-step kernel env =
    trans
      (runRect2Interchange-unrelated
        (tailCols kernel)
        (runCol env (kernelProgram (base kernel)) fzero)
        other
        output≢other
        j)
      (runCol-unrelated kernel env fzero other output≢other j)

runRect2Interchange-preserve-target :
  ∀ {rows cols} →
  (kernel : WholeRect2Kernel rows cols) →
  (env : Env rank2) →
  (target : RectIx (shape (outputArr (base kernel)))) →
  (noHit : ∀ row col → resolve (outputAt (base kernel) (row , col)) ≢ target) →
  lookupEnv (runRect2Interchange env (kernelProgram (base kernel))) (outputArr (base kernel)) target ≡
    lookupEnv env (outputArr (base kernel)) target
runRect2Interchange-preserve-target {rows} {zero} kernel env target noHit = refl
runRect2Interchange-preserve-target {rows} {suc cols} kernel env target noHit
  rewrite runRect2Interchange-step kernel env =
    trans
      (runRect2Interchange-preserve-target
        (tailCols kernel)
        (runCol env (kernelProgram (base kernel)) fzero)
        target
        (λ row col → noHit row (fsuc col)))
      (runCol-preserve-target kernel env fzero target (λ row → noHit row fzero))

runRect2Interchange-pointwise :
  ∀ {rows cols} →
  (kernel : WholeRect2Kernel rows cols) →
  (env : Env rank2) →
  (row : Fin rows) →
  (col : Fin cols) →
  lookupEnv (runRect2Interchange env (kernelProgram (base kernel))) (outputArr (base kernel))
    (resolve (outputAt (base kernel) (row , col))) ≡
    transform (base kernel)
      (lookupEnv env (inputArr (base kernel)) (resolve (inputAt (base kernel) (row , col))))
runRect2Interchange-pointwise {rows} {zero} kernel env row ()
runRect2Interchange-pointwise {rows} {suc cols} kernel env row fzero
  rewrite runRect2Interchange-step kernel env =
    trans
      (runRect2Interchange-preserve-target
        (tailCols kernel)
        (runCol env (kernelProgram (base kernel)) fzero)
        (resolve (outputAt (base kernel) (row , fzero)))
        (λ row′ col eq → fsuc≢fzero (cong proj₂ (outputUnique kernel eq))))
      (runCol-pointwise kernel env row fzero)
runRect2Interchange-pointwise {rows} {suc cols} kernel env row (fsuc col)
  rewrite runRect2Interchange-step kernel env =
    trans
      (runRect2Interchange-pointwise
        (tailCols kernel)
        (runCol env (kernelProgram (base kernel)) fzero)
        row
        col)
      (cong
        (transform (base kernel))
        (runCol-input-preserved
          kernel
          env
          fzero
          (resolve (inputAt (base kernel) (row , fsuc col)))))

runRect2Interchange-state :
  ∀ {rows cols} →
  (kernel : ExactCoverRect2Kernel rows cols) →
  (env : Env rank2) →
  (arr : Array rank2) →
  (ix : RectIx (shape arr)) →
  lookupEnv (runRect2Interchange env (kernelProgram (base (wholeKernel kernel)))) arr ix ≡
    rect2Expected kernel env arr ix
runRect2Interchange-state kernel env arr ix with arrayEq arr (outputArr (base (wholeKernel kernel)))
runRect2Interchange-state kernel env arr ix | yes refl =
  trans
    (cong
      (lookupEnv (runRect2Interchange env (kernelProgram (base (wholeKernel kernel)))) (outputArr (base (wholeKernel kernel))))
      (sym (outputCovered kernel ix)))
    (runRect2Interchange-pointwise
      (wholeKernel kernel)
      env
      (coverRow kernel ix)
      (coverCol kernel ix))
runRect2Interchange-state kernel env arr ix | no arr≢output =
  runRect2Interchange-unrelated
    (wholeKernel kernel)
    env
    arr
    (λ eq → arr≢output (sym eq))
    ix

loop-interchange-preserves-state :
  ∀ {rows cols} →
  (kernel : ExactCoverRect2Kernel rows cols) →
  (env : Env rank2) →
  PostStateEq
    (runRect2 env (kernelProgram (base (wholeKernel kernel))))
    (runRect2Interchange env (kernelProgram (base (wholeKernel kernel))))
loop-interchange-preserves-state kernel env arr ix =
  trans
    (runRect2-state kernel env arr ix)
    (sym (runRect2Interchange-state kernel env arr ix))
