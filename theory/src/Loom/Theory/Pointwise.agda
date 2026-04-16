{-# OPTIONS --safe #-}

module Loom.Theory.Pointwise where

open import Loom.Theory.Access
open import Loom.Theory.Index
open import Loom.Theory.Prelude
open import Loom.Theory.Schedule
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.Syntax as Syn

record PointwiseKernel {rank : Rank} (sched : Schedule rank) (dom : Shape rank) : Set where
  field
    inputArr : Array rank
    outputArr : Array rank
    distinct : inputArr ≢ outputArr
    inputAt : Index sched dom → Access ReadOnly sched inputArr
    outputAt : Index sched dom → Access WriteOnly sched outputArr
    transform : ℕ → ℕ

  kernelProgram : Syn.Program sched dom
  kernelProgram =
    Syn.program λ ix →
      Syn.read read-only (inputAt ix) λ value →
        Syn.write write-only (outputAt ix) (transform value)

open PointwiseKernel public

runAt-pointwise :
  ∀ {rank sched dom} →
  (kernel : PointwiseKernel {rank} sched dom) →
  (env : Env rank) →
  (ix : Index sched dom) →
  lookupEnv (runAt env (kernelProgram kernel) ix) (outputArr kernel) (resolve (outputAt kernel ix)) ≡
    transform kernel (lookupEnv env (inputArr kernel) (resolve (inputAt kernel ix)))
runAt-pointwise kernel env ix =
  updateEnv-same
    env
    (outputArr kernel)
    (outputAt kernel ix)
    (transform kernel (lookupEnv env (inputArr kernel) (resolve (inputAt kernel ix))))

runAt-unrelated :
  ∀ {rank sched dom} →
  (kernel : PointwiseKernel {rank} sched dom) →
  (env : Env rank) →
  (ix : Index sched dom) →
  (other : Array rank) →
  outputArr kernel ≢ other →
  (j : RectIx (shape other)) →
  lookupEnv (runAt env (kernelProgram kernel) ix) other j ≡ lookupEnv env other j
runAt-unrelated kernel env ix other output≢other j =
  updateEnv-other
    env
    (outputArr kernel)
    other
    (outputAt kernel ix)
    (transform kernel (lookupEnv env (inputArr kernel) (resolve (inputAt kernel ix))))
    output≢other
    j

runAt-input-preserved :
  ∀ {rank sched dom} →
  (kernel : PointwiseKernel {rank} sched dom) →
  (env : Env rank) →
  (ix : Index sched dom) →
  (j : RectIx (shape (inputArr kernel))) →
  lookupEnv (runAt env (kernelProgram kernel) ix) (inputArr kernel) j ≡ lookupEnv env (inputArr kernel) j
runAt-input-preserved kernel env ix j =
  runAt-unrelated kernel env ix (inputArr kernel) (λ eq → distinct kernel (sym eq)) j
