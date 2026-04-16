{-# OPTIONS --safe #-}

module Loom.Theory.SchedulePreservation where

open import Loom.Theory.ExactCoverTiled
open import Loom.Theory.Index
open import Loom.Theory.Prelude
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.Traversal using (LinearTraversal; stepAt)
open import Loom.Theory.TiledPointwise
open import Loom.Theory.WholeTiled

record ExactCoverTraversal
  {dom tileShape : Shape rank2}
  (kernel : TiledPointwiseKernel dom tileShape)
  (n : ℕ) : Set where
  field
    steps : LinearTraversal {A = TileIx dom tileShape} n
    outputUnique :
      ∀ {i j} →
      outputGlobal kernel (stepAt steps i) ≡ outputGlobal kernel (stepAt steps j) →
      i ≡ j
    coverIndex :
      RectIx (shape (outputArr kernel)) → Fin n
    outputCovered :
      (target : RectIx (shape (outputArr kernel))) →
      outputGlobal kernel (stepAt steps (coverIndex target)) ≡ target

open ExactCoverTraversal public

traversalWholeKernel :
  ∀ {dom tileShape n} →
  (kernel : TiledPointwiseKernel dom tileShape) →
  ExactCoverTraversal kernel n →
  WholeTiledKernel n dom tileShape
traversalWholeKernel kernel traversal = record
  { baseKernel = kernel
  ; steps = steps traversal
  ; outputUnique = outputUnique traversal
  }

traversalExactKernel :
  ∀ {dom tileShape n} →
  (kernel : TiledPointwiseKernel dom tileShape) →
  ExactCoverTraversal kernel n →
  ExactCoverTiledKernel n dom tileShape
traversalExactKernel kernel traversal = record
  { wholeKernel = traversalWholeKernel kernel traversal
  ; coverIndex = coverIndex traversal
  ; outputCovered = outputCovered traversal
  }

record EquivalentTraversals
  {dom tileShape : Shape rank2}
  (kernel : TiledPointwiseKernel dom tileShape)
  (n m : ℕ) : Set where
  field
    left : ExactCoverTraversal kernel n
    right : ExactCoverTraversal kernel m
    sameInput :
      (target : RectIx (shape (outputArr kernel))) →
      inputGlobal kernel (stepAt (steps left) (coverIndex left target)) ≡
      inputGlobal kernel (stepAt (steps right) (coverIndex right target))

open EquivalentTraversals public

schedule-preserves-output :
  ∀ {dom tileShape n m} →
  (kernel : TiledPointwiseKernel dom tileShape) →
  (eqv : EquivalentTraversals kernel n m) →
  (env : Env rank2) →
  (target : RectIx (shape (outputArr kernel))) →
  lookupEnv (runTiles env (traversalWholeKernel kernel (left eqv))) (outputArr kernel) target ≡
    lookupEnv (runTiles env (traversalWholeKernel kernel (right eqv))) (outputArr kernel) target
schedule-preserves-output kernel eqv env target =
  trans
    (runTiles-covered-pointwise (traversalExactKernel kernel (left eqv)) env target)
    (trans
      (cong
        (λ j → transform kernel (lookupEnv env (inputArr kernel) j))
        (sameInput eqv target))
      (sym (runTiles-covered-pointwise (traversalExactKernel kernel (right eqv)) env target)))

schedule-preserves-input :
  ∀ {dom tileShape n m} →
  (kernel : TiledPointwiseKernel dom tileShape) →
  (eqv : EquivalentTraversals kernel n m) →
  (env : Env rank2) →
  (j : RectIx (shape (inputArr kernel))) →
  lookupEnv (runTiles env (traversalWholeKernel kernel (left eqv))) (inputArr kernel) j ≡
    lookupEnv (runTiles env (traversalWholeKernel kernel (right eqv))) (inputArr kernel) j
schedule-preserves-input kernel eqv env j =
  trans
    (runTiles-covered-input-preserved (traversalExactKernel kernel (left eqv)) env j)
    (sym (runTiles-covered-input-preserved (traversalExactKernel kernel (right eqv)) env j))
