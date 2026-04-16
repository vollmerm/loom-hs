{-# OPTIONS --safe #-}

module Loom.Theory.WholeRect1 where

open import Loom.Theory.Access
open import Loom.Theory.Index
open import Loom.Theory.Pointwise
open import Loom.Theory.Prelude
open import Loom.Theory.RectExecution
open import Loom.Theory.Schedule
open import Loom.Theory.Semantics
open import Loom.Theory.Shape

record WholeRect1Kernel (n : ℕ) : Set where
  field
    base : PointwiseKernel rect (shape1 n)
    outputUnique :
      ∀ {i j} →
      resolve (outputAt base i) ≡ resolve (outputAt base j) →
      i ≡ j

open WholeRect1Kernel public

fsuc-injective : ∀ {n} {i j : Fin n} → fsuc i ≡ fsuc j → i ≡ j
fsuc-injective refl = refl

fsuc≢fzero : ∀ {n} {i : Fin n} → fsuc i ≢ fzero
fsuc≢fzero ()

tailKernel : ∀ {n} → WholeRect1Kernel (suc n) → WholeRect1Kernel n
tailKernel kernel = record
  { base = record
      { inputArr = inputArr (base kernel)
      ; outputArr = outputArr (base kernel)
      ; distinct = distinct (base kernel)
      ; inputAt = λ i → inputAt (base kernel) (fsuc i)
      ; outputAt = λ i → outputAt (base kernel) (fsuc i)
      ; transform = transform (base kernel)
      }
  ; outputUnique = λ eq → fsuc-injective (outputUnique kernel eq)
  }

runRect1-step :
  ∀ {n} →
  (kernel : WholeRect1Kernel (suc n)) →
  (env : Env rank1) →
  runRect1 env (kernelProgram (base kernel)) ≡
    runRect1
      (runAt env (kernelProgram (base kernel)) fzero)
      (kernelProgram (base (tailKernel kernel)))
runRect1-step kernel env = refl

runRect1-unrelated :
  ∀ {n} →
  (kernel : WholeRect1Kernel n) →
  (env : Env rank1) →
  (other : Array rank1) →
  outputArr (base kernel) ≢ other →
  (j : RectIx (shape other)) →
  lookupEnv (runRect1 env (kernelProgram (base kernel))) other j ≡ lookupEnv env other j
runRect1-unrelated {zero} kernel env other output≢other j = refl
runRect1-unrelated {suc n} kernel env other output≢other j
  rewrite runRect1-step kernel env =
    trans
      (runRect1-unrelated
        (tailKernel kernel)
        (runAt env (kernelProgram (base kernel)) fzero)
        other
        output≢other
        j)
      (runAt-unrelated (base kernel) env fzero other output≢other j)

runRect1-preserve-target :
  ∀ {n} →
  (kernel : WholeRect1Kernel n) →
  (env : Env rank1) →
  (target : RectIx (shape (outputArr (base kernel)))) →
  (noHit : ∀ i → resolve (outputAt (base kernel) i) ≢ target) →
  lookupEnv (runRect1 env (kernelProgram (base kernel))) (outputArr (base kernel)) target ≡
    lookupEnv env (outputArr (base kernel)) target
runRect1-preserve-target {zero} kernel env target noHit = refl
runRect1-preserve-target {suc n} kernel env target noHit
  rewrite runRect1-step kernel env =
    trans
      (runRect1-preserve-target
        (tailKernel kernel)
        (runAt env (kernelProgram (base kernel)) fzero)
        target
        (λ i → noHit (fsuc i)))
      (updateEnv-other-index
        env
        (outputArr (base kernel))
        (outputAt (base kernel) fzero)
        (transform (base kernel)
          (lookupEnv env (inputArr (base kernel))
            (resolve (inputAt (base kernel) fzero))))
        target
        (noHit fzero))

runRect1-pointwise :
  ∀ {n} →
  (kernel : WholeRect1Kernel n) →
  (env : Env rank1) →
  (i : Fin n) →
  lookupEnv (runRect1 env (kernelProgram (base kernel))) (outputArr (base kernel))
    (resolve (outputAt (base kernel) i)) ≡
    transform (base kernel)
      (lookupEnv env (inputArr (base kernel)) (resolve (inputAt (base kernel) i)))
runRect1-pointwise {zero} kernel env ()
runRect1-pointwise {suc n} kernel env fzero
  rewrite runRect1-step kernel env =
    trans
      (runRect1-preserve-target
        (tailKernel kernel)
        (runAt env (kernelProgram (base kernel)) fzero)
        (resolve (outputAt (base kernel) fzero))
        (λ i eq → fsuc≢fzero (outputUnique kernel eq)))
      (runAt-pointwise (base kernel) env fzero)
runRect1-pointwise {suc n} kernel env (fsuc i)
  rewrite runRect1-step kernel env =
    trans
      (runRect1-pointwise
        (tailKernel kernel)
        (runAt env (kernelProgram (base kernel)) fzero)
        i)
      (cong
        (transform (base kernel))
        (runAt-input-preserved
          (base kernel)
          env
          fzero
          (resolve (inputAt (base kernel) (fsuc i)))))

runRect1-input-preserved :
  ∀ {n} →
  (kernel : WholeRect1Kernel n) →
  (env : Env rank1) →
  (j : RectIx (shape (inputArr (base kernel)))) →
  lookupEnv (runRect1 env (kernelProgram (base kernel))) (inputArr (base kernel)) j ≡
    lookupEnv env (inputArr (base kernel)) j
runRect1-input-preserved kernel env j =
  runRect1-unrelated kernel env (inputArr (base kernel)) (λ eq → distinct (base kernel) (sym eq)) j
