{-# OPTIONS --safe #-}

module Loom.Theory.WholeRect2 where

open import Loom.Theory.Access
open import Loom.Theory.Index
open import Loom.Theory.Pointwise
open import Loom.Theory.Prelude
open import Loom.Theory.RectExecution
open import Loom.Theory.Schedule
open import Loom.Theory.Semantics
open import Loom.Theory.Shape

record WholeRect2Kernel (rows cols : ℕ) : Set where
  field
    base : PointwiseKernel rect (shape2 rows cols)
    outputUnique :
      ∀ {i j i′ j′} →
      resolve (outputAt base (i , j)) ≡ resolve (outputAt base (i′ , j′)) →
      (i , j) ≡ (i′ , j′)

open WholeRect2Kernel public

fsuc-injective : ∀ {n} {i j : Fin n} → fsuc i ≡ fsuc j → i ≡ j
fsuc-injective refl = refl

pair-fsuc-col-injective :
  ∀ {rows cols} {i i′ : Fin rows} {j j′ : Fin cols} →
  (i , fsuc j) ≡ (i′ , fsuc j′) →
  (i , j) ≡ (i′ , j′)
pair-fsuc-col-injective {i = i} {i′} {j} {j′} eq
  rewrite cong proj₁ eq | fsuc-injective (cong proj₂ eq) = refl

pair-fsuc-row-injective :
  ∀ {rows cols} {i i′ : Fin rows} {j j′ : Fin cols} →
  (fsuc i , j) ≡ (fsuc i′ , j′) →
  (i , j) ≡ (i′ , j′)
pair-fsuc-row-injective {i = i} {i′} {j} {j′} eq
  rewrite fsuc-injective (cong proj₁ eq) | cong proj₂ eq = refl

fsuc≢fzero : ∀ {n} {i : Fin n} → fsuc i ≢ fzero
fsuc≢fzero ()

tailCols :
  ∀ {rows cols} →
  WholeRect2Kernel rows (suc cols) →
  WholeRect2Kernel rows cols
tailCols kernel = record
  { base = record
      { inputArr = inputArr (base kernel)
      ; outputArr = outputArr (base kernel)
      ; distinct = distinct (base kernel)
      ; inputAt = λ where
          (row , col) → inputAt (base kernel) (row , fsuc col)
      ; outputAt = λ where
          (row , col) → outputAt (base kernel) (row , fsuc col)
      ; transform = transform (base kernel)
      }
  ; outputUnique = λ eq → pair-fsuc-col-injective (outputUnique kernel eq)
  }

tailRows :
  ∀ {rows cols} →
  WholeRect2Kernel (suc rows) cols →
  WholeRect2Kernel rows cols
tailRows kernel = record
  { base = record
      { inputArr = inputArr (base kernel)
      ; outputArr = outputArr (base kernel)
      ; distinct = distinct (base kernel)
      ; inputAt = λ where
          (row , col) → inputAt (base kernel) (fsuc row , col)
      ; outputAt = λ where
          (row , col) → outputAt (base kernel) (fsuc row , col)
      ; transform = transform (base kernel)
      }
  ; outputUnique = λ eq → pair-fsuc-row-injective (outputUnique kernel eq)
  }

runRow-step :
  ∀ {rows cols} →
  (kernel : WholeRect2Kernel rows (suc cols)) →
  (env : Env rank2) →
  (row : Fin rows) →
  runRow env (kernelProgram (base kernel)) row ≡
    runRow
      (runAt env (kernelProgram (base kernel)) (row , fzero))
      (kernelProgram (base (tailCols kernel)))
      row
runRow-step kernel env row = refl

runRow-unrelated :
  ∀ {rows cols} →
  (kernel : WholeRect2Kernel rows cols) →
  (env : Env rank2) →
  (row : Fin rows) →
  (other : Array rank2) →
  outputArr (base kernel) ≢ other →
  (j : RectIx (shape other)) →
  lookupEnv (runRow env (kernelProgram (base kernel)) row) other j ≡ lookupEnv env other j
runRow-unrelated {rows} {zero} kernel env row other output≢other j = refl
runRow-unrelated {rows} {suc cols} kernel env row other output≢other j
  rewrite runRow-step kernel env row =
    trans
      (runRow-unrelated
        (tailCols kernel)
        (runAt env (kernelProgram (base kernel)) (row , fzero))
        row
        other
        output≢other
        j)
      (runAt-unrelated (base kernel) env (row , fzero) other output≢other j)

runRow-preserve-target :
  ∀ {rows cols} →
  (kernel : WholeRect2Kernel rows cols) →
  (env : Env rank2) →
  (row : Fin rows) →
  (target : RectIx (shape (outputArr (base kernel)))) →
  (noHit : ∀ col → resolve (outputAt (base kernel) (row , col)) ≢ target) →
  lookupEnv (runRow env (kernelProgram (base kernel)) row) (outputArr (base kernel)) target ≡
    lookupEnv env (outputArr (base kernel)) target
runRow-preserve-target {rows} {zero} kernel env row target noHit = refl
runRow-preserve-target {rows} {suc cols} kernel env row target noHit
  rewrite runRow-step kernel env row =
    trans
      (runRow-preserve-target
        (tailCols kernel)
        (runAt env (kernelProgram (base kernel)) (row , fzero))
        row
        target
        (λ col → noHit (fsuc col)))
      (updateEnv-other-index
        env
        (outputArr (base kernel))
        (outputAt (base kernel) (row , fzero))
        (transform (base kernel)
          (lookupEnv env (inputArr (base kernel))
            (resolve (inputAt (base kernel) (row , fzero)))))
        target
        (noHit fzero))

runRow-pointwise :
  ∀ {rows cols} →
  (kernel : WholeRect2Kernel rows cols) →
  (env : Env rank2) →
  (row : Fin rows) →
  (col : Fin cols) →
  lookupEnv (runRow env (kernelProgram (base kernel)) row) (outputArr (base kernel))
    (resolve (outputAt (base kernel) (row , col))) ≡
    transform (base kernel)
      (lookupEnv env (inputArr (base kernel)) (resolve (inputAt (base kernel) (row , col))))
runRow-pointwise {rows} {zero} kernel env row ()
runRow-pointwise {rows} {suc cols} kernel env row fzero
  rewrite runRow-step kernel env row =
    trans
      (runRow-preserve-target
        (tailCols kernel)
        (runAt env (kernelProgram (base kernel)) (row , fzero))
        row
        (resolve (outputAt (base kernel) (row , fzero)))
        (λ col eq → fsuc≢fzero (cong proj₂ (outputUnique kernel eq))))
      (runAt-pointwise (base kernel) env (row , fzero))
runRow-pointwise {rows} {suc cols} kernel env row (fsuc col)
  rewrite runRow-step kernel env row =
    trans
      (runRow-pointwise
        (tailCols kernel)
        (runAt env (kernelProgram (base kernel)) (row , fzero))
        row
        col)
      (cong
        (transform (base kernel))
        (runAt-input-preserved
          (base kernel)
          env
          (row , fzero)
          (resolve (inputAt (base kernel) (row , fsuc col)))))

runRow-input-preserved :
  ∀ {rows cols} →
  (kernel : WholeRect2Kernel rows cols) →
  (env : Env rank2) →
  (row : Fin rows) →
  (j : RectIx (shape (inputArr (base kernel)))) →
  lookupEnv (runRow env (kernelProgram (base kernel)) row) (inputArr (base kernel)) j ≡
    lookupEnv env (inputArr (base kernel)) j
runRow-input-preserved kernel env row j =
  runRow-unrelated kernel env row (inputArr (base kernel)) (λ eq → distinct (base kernel) (sym eq)) j

runRect2-step :
  ∀ {rows cols} →
  (kernel : WholeRect2Kernel (suc rows) cols) →
  (env : Env rank2) →
  runRect2 env (kernelProgram (base kernel)) ≡
    runRect2
      (runRow env (kernelProgram (base kernel)) fzero)
      (kernelProgram (base (tailRows kernel)))
runRect2-step kernel env = refl

runRect2-unrelated :
  ∀ {rows cols} →
  (kernel : WholeRect2Kernel rows cols) →
  (env : Env rank2) →
  (other : Array rank2) →
  outputArr (base kernel) ≢ other →
  (j : RectIx (shape other)) →
  lookupEnv (runRect2 env (kernelProgram (base kernel))) other j ≡ lookupEnv env other j
runRect2-unrelated {zero} kernel env other output≢other j = refl
runRect2-unrelated {suc rows} kernel env other output≢other j
  rewrite runRect2-step kernel env =
    trans
      (runRect2-unrelated
        (tailRows kernel)
        (runRow env (kernelProgram (base kernel)) fzero)
        other
        output≢other
        j)
      (runRow-unrelated kernel env fzero other output≢other j)

runRect2-preserve-target :
  ∀ {rows cols} →
  (kernel : WholeRect2Kernel rows cols) →
  (env : Env rank2) →
  (target : RectIx (shape (outputArr (base kernel)))) →
  (noHit : ∀ row col → resolve (outputAt (base kernel) (row , col)) ≢ target) →
  lookupEnv (runRect2 env (kernelProgram (base kernel))) (outputArr (base kernel)) target ≡
    lookupEnv env (outputArr (base kernel)) target
runRect2-preserve-target {zero} kernel env target noHit = refl
runRect2-preserve-target {suc rows} kernel env target noHit
  rewrite runRect2-step kernel env =
    trans
      (runRect2-preserve-target
        (tailRows kernel)
        (runRow env (kernelProgram (base kernel)) fzero)
        target
        (λ row col → noHit (fsuc row) col))
      (runRow-preserve-target kernel env fzero target (λ col → noHit fzero col))

runRect2-pointwise :
  ∀ {rows cols} →
  (kernel : WholeRect2Kernel rows cols) →
  (env : Env rank2) →
  (row : Fin rows) →
  (col : Fin cols) →
  lookupEnv (runRect2 env (kernelProgram (base kernel))) (outputArr (base kernel))
    (resolve (outputAt (base kernel) (row , col))) ≡
    transform (base kernel)
      (lookupEnv env (inputArr (base kernel)) (resolve (inputAt (base kernel) (row , col))))
runRect2-pointwise {zero} kernel env () col
runRect2-pointwise {suc rows} kernel env fzero col
  rewrite runRect2-step kernel env =
    trans
      (runRect2-preserve-target
        (tailRows kernel)
        (runRow env (kernelProgram (base kernel)) fzero)
        (resolve (outputAt (base kernel) (fzero , col)))
        (λ row col′ eq → fsuc≢fzero (cong proj₁ (outputUnique kernel eq))))
      (runRow-pointwise kernel env fzero col)
runRect2-pointwise {suc rows} kernel env (fsuc row) col
  rewrite runRect2-step kernel env =
    trans
      (runRect2-pointwise
        (tailRows kernel)
        (runRow env (kernelProgram (base kernel)) fzero)
        row
        col)
      (cong
        (transform (base kernel))
        (runRow-input-preserved
          kernel
          env
          fzero
          (resolve (inputAt (base kernel) (fsuc row , col)))))

runRect2-input-preserved :
  ∀ {rows cols} →
  (kernel : WholeRect2Kernel rows cols) →
  (env : Env rank2) →
  (j : RectIx (shape (inputArr (base kernel)))) →
  lookupEnv (runRect2 env (kernelProgram (base kernel))) (inputArr (base kernel)) j ≡
    lookupEnv env (inputArr (base kernel)) j
runRect2-input-preserved kernel env j =
  runRect2-unrelated kernel env (inputArr (base kernel)) (λ eq → distinct (base kernel) (sym eq)) j
