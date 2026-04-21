{-# OPTIONS --safe #-}

module Loom.Theory.WholeLinear where

open import Loom.Theory.Access
open import Loom.Theory.Index
open import Loom.Theory.Pointwise
open import Loom.Theory.Prelude
open import Loom.Theory.Schedule
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.Traversal
  using (LinearTraversal; runEnumerator; stepAt; traversal)
import Loom.Theory.Traversal as Traversal

-- A whole-program kernel whose iteration domain is a LinearTraversal.
-- Both 1D rectangular kernels (steps = id) and explicit tiled kernels
-- (steps over TileIx) are instances of this record.
record WholeKernel
    {rank : Rank} {sched : Schedule rank} {dom : Shape rank}
    (n : ℕ) : Set where
  field
    base     : PointwiseKernel sched dom
    steps    : LinearTraversal {A = Index sched dom} n
    outputUnique :
      ∀ {i j} →
      resolve (outputAt base (stepAt steps i)) ≡
      resolve (outputAt base (stepAt steps j)) →
      i ≡ j

open WholeKernel public

runWhole :
  ∀ {rank sched dom n} →
  Env rank →
  WholeKernel {rank} {sched} {dom} n →
  Env rank
runWhole env kernel =
  runEnumerator env (kernelProgram (base kernel)) (traversal (steps kernel))

private
  fsuc-injective : ∀ {n} {i j : Fin n} → fsuc i ≡ fsuc j → i ≡ j
  fsuc-injective refl = refl

  fsuc≢fzero : ∀ {n} {i : Fin n} → fsuc i ≢ fzero
  fsuc≢fzero ()

tailKernel :
  ∀ {rank sched dom n} →
  WholeKernel {rank} {sched} {dom} (suc n) →
  WholeKernel {rank} {sched} {dom} n
tailKernel kernel = record
  { base     = base kernel
  ; steps    = Traversal.linearTraversal (λ i → stepAt (steps kernel) (fsuc i))
  ; outputUnique = λ eq → fsuc-injective (outputUnique kernel eq)
  }

runWhole-step :
  ∀ {rank sched dom n} →
  (kernel : WholeKernel {rank} {sched} {dom} (suc n)) →
  (env : Env rank) →
  runWhole env kernel ≡
    runWhole
      (runAt env (kernelProgram (base kernel)) (stepAt (steps kernel) fzero))
      (tailKernel kernel)
runWhole-step kernel env = refl

runWhole-unrelated :
  ∀ {rank sched dom n} →
  (kernel : WholeKernel {rank} {sched} {dom} n) →
  (env : Env rank) →
  (other : Array rank) →
  outputArr (base kernel) ≢ other →
  (j : RectIx (shape other)) →
  lookupEnv (runWhole env kernel) other j ≡ lookupEnv env other j
runWhole-unrelated {n = zero}  kernel env other _            j = refl
runWhole-unrelated {n = suc n} kernel env other output≢other j
  rewrite runWhole-step kernel env =
    trans
      (runWhole-unrelated
        (tailKernel kernel)
        (runAt env (kernelProgram (base kernel)) (stepAt (steps kernel) fzero))
        other
        output≢other
        j)
      (runAt-unrelated
        (base kernel) env (stepAt (steps kernel) fzero) other output≢other j)

runWhole-preserve-target :
  ∀ {rank sched dom n} →
  (kernel : WholeKernel {rank} {sched} {dom} n) →
  (env : Env rank) →
  (target : RectIx (shape (outputArr (base kernel)))) →
  (noHit : ∀ i → resolve (outputAt (base kernel) (stepAt (steps kernel) i)) ≢ target) →
  lookupEnv (runWhole env kernel) (outputArr (base kernel)) target ≡
    lookupEnv env (outputArr (base kernel)) target
runWhole-preserve-target {n = zero}  kernel env target noHit = refl
runWhole-preserve-target {n = suc n} kernel env target noHit
  rewrite runWhole-step kernel env =
    trans
      (runWhole-preserve-target
        (tailKernel kernel)
        (runAt env (kernelProgram (base kernel)) (stepAt (steps kernel) fzero))
        target
        (λ i → noHit (fsuc i)))
      (updateEnv-other-index
        env
        (outputArr (base kernel))
        (outputAt (base kernel) (stepAt (steps kernel) fzero))
        (transform (base kernel)
          (lookupEnv env (inputArr (base kernel))
            (resolve (inputAt (base kernel) (stepAt (steps kernel) fzero)))))
        target
        (noHit fzero))

runWhole-pointwise :
  ∀ {rank sched dom n} →
  (kernel : WholeKernel {rank} {sched} {dom} n) →
  (env : Env rank) →
  (i : Fin n) →
  lookupEnv (runWhole env kernel) (outputArr (base kernel))
    (resolve (outputAt (base kernel) (stepAt (steps kernel) i))) ≡
    transform (base kernel)
      (lookupEnv env (inputArr (base kernel))
        (resolve (inputAt (base kernel) (stepAt (steps kernel) i))))
runWhole-pointwise {n = zero}  kernel env ()
runWhole-pointwise {n = suc n} kernel env fzero
  rewrite runWhole-step kernel env =
    trans
      (runWhole-preserve-target
        (tailKernel kernel)
        (runAt env (kernelProgram (base kernel)) (stepAt (steps kernel) fzero))
        (resolve (outputAt (base kernel) (stepAt (steps kernel) fzero)))
        (λ i eq → fsuc≢fzero (outputUnique kernel eq)))
      (runAt-pointwise (base kernel) env (stepAt (steps kernel) fzero))
runWhole-pointwise {n = suc n} kernel env (fsuc i)
  rewrite runWhole-step kernel env =
    trans
      (runWhole-pointwise
        (tailKernel kernel)
        (runAt env (kernelProgram (base kernel)) (stepAt (steps kernel) fzero))
        i)
      (cong
        (transform (base kernel))
        (runAt-input-preserved
          (base kernel)
          env
          (stepAt (steps kernel) fzero)
          (resolve (inputAt (base kernel) (stepAt (steps kernel) (fsuc i))))))

runWhole-input-preserved :
  ∀ {rank sched dom n} →
  (kernel : WholeKernel {rank} {sched} {dom} n) →
  (env : Env rank) →
  (j : RectIx (shape (inputArr (base kernel)))) →
  lookupEnv (runWhole env kernel) (inputArr (base kernel)) j ≡
    lookupEnv env (inputArr (base kernel)) j
runWhole-input-preserved kernel env j =
  runWhole-unrelated
    kernel env (inputArr (base kernel)) (λ eq → distinct (base kernel) (sym eq)) j
