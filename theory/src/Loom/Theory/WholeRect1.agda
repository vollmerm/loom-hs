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
open import Loom.Theory.WholeLinear
  using (WholeKernel; runWhole;
         runWhole-unrelated; runWhole-pointwise; runWhole-input-preserved)
import Loom.Theory.Traversal as Traversal

-- A 1D rectangular kernel whose n steps are implicitly id (index 0..n-1).
-- An instance of WholeLinear.WholeKernel with sched=rect, dom=shape1 n, steps=id.
record WholeRect1Kernel (n : ℕ) : Set where
  field
    base : PointwiseKernel rect (shape1 n)
    outputUnique :
      ∀ {i j} →
      resolve (outputAt base i) ≡ resolve (outputAt base j) →
      i ≡ j

open WholeRect1Kernel public

-- Convert to the generic WholeKernel; runRect1 = runWhole by refl.
toWholeKernel : ∀ {n} → WholeRect1Kernel n → WholeKernel {rank1} {rect} {shape1 n} n
toWholeKernel kernel = record
  { base         = base kernel
  ; steps        = Traversal.linearTraversal (λ i → i)
  ; outputUnique = outputUnique kernel
  }

-- runRect1 env prog  and  runWhole env (toWholeKernel k)  are definitionally equal.
runRect1≡runWhole :
  ∀ {n} (kernel : WholeRect1Kernel n) (env : Env rank1) →
  runRect1 env (kernelProgram (base kernel)) ≡
    runWhole env (toWholeKernel kernel)
runRect1≡runWhole kernel env = refl

runRect1-unrelated :
  ∀ {n} →
  (kernel : WholeRect1Kernel n) →
  (env : Env rank1) →
  (other : Array rank1) →
  outputArr (base kernel) ≢ other →
  (j : RectIx (shape other)) →
  lookupEnv (runRect1 env (kernelProgram (base kernel))) other j ≡ lookupEnv env other j
runRect1-unrelated kernel env other output≢other j =
  runWhole-unrelated (toWholeKernel kernel) env other output≢other j

runRect1-pointwise :
  ∀ {n} →
  (kernel : WholeRect1Kernel n) →
  (env : Env rank1) →
  (i : Fin n) →
  lookupEnv (runRect1 env (kernelProgram (base kernel))) (outputArr (base kernel))
    (resolve (outputAt (base kernel) i)) ≡
    transform (base kernel)
      (lookupEnv env (inputArr (base kernel)) (resolve (inputAt (base kernel) i)))
runRect1-pointwise kernel env i =
  runWhole-pointwise (toWholeKernel kernel) env i

runRect1-input-preserved :
  ∀ {n} →
  (kernel : WholeRect1Kernel n) →
  (env : Env rank1) →
  (j : RectIx (shape (inputArr (base kernel)))) →
  lookupEnv (runRect1 env (kernelProgram (base kernel))) (inputArr (base kernel)) j ≡
    lookupEnv env (inputArr (base kernel)) j
runRect1-input-preserved kernel env j =
  runWhole-input-preserved (toWholeKernel kernel) env j
