{-# OPTIONS --safe #-}

-- Connects the output of runRect1 to a direct fold over the
-- transformed inputs.  This is the formal justification for the
-- foldFor / accumDVecFor optimisation in the Loom library: passing
-- the accumulator as a loop argument (Haskell unboxed register) is
-- semantically equivalent to folding the output array produced by
-- a whole-run kernel.
--
-- KEY THEOREM  (foldFor-correct)
--
--   foldArray1D r (runRect1 env prog) outputArr
--     ≡ foldArray1D r (rect1Expected kernel env) outputArr
--
--   Proof: foldArray1D-cong applied to runRect1-state, which gives
--   pointwise equality of the two environments on the output array.
--   The real content lives in ExactCoverRect1 and ConfluenceReduction.

module Loom.Theory.FoldForCorrect where

open import Loom.Theory.ConfluenceReduction using (foldReducerFrom-cong)
open import Loom.Theory.ExactCoverRect1
open import Loom.Theory.Pointwise
open import Loom.Theory.Prelude
open import Loom.Theory.Reduction
open import Loom.Theory.RectExecution
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.WholeRect1

-- ─────────────────────────────────────────────────────────────────────────────
-- Bridge lemma
-- ─────────────────────────────────────────────────────────────────────────────

-- foldArray1D is definitionally equal to foldReducer after the array
-- constructor is in scope.  The explicit patterns let Agda reduce the match.
foldArray1D-eq-foldReducer :
  ∀ (r : Reducer) (env : Env rank1) (ident n : ℕ) →
  foldArray1D r env (array ident (shape1 n)) ≡
  foldReducer r (lookupEnv env (array ident (shape1 n)))
foldArray1D-eq-foldReducer r env ident n = refl

-- Congruence lemma for foldArray1D: if two environments agree pointwise on
-- an array, they produce the same fold result.  Pattern-matching on the array
-- constructor here lets Agda reduce foldArray1D to foldReducer.
foldArray1D-cong :
  ∀ (r : Reducer) (env₁ env₂ : Env rank1) (arr : Array rank1) →
  (∀ ix → lookupEnv env₁ arr ix ≡ lookupEnv env₂ arr ix) →
  foldArray1D r env₁ arr ≡ foldArray1D r env₂ arr
foldArray1D-cong r env₁ env₂ (array ident (shape1 n)) eq =
  cong (done r) (foldReducerFrom-cong r (init r) eq)

-- ─────────────────────────────────────────────────────────────────────────────
-- Main theorem: foldFor correctness
-- ─────────────────────────────────────────────────────────────────────────────

-- After running a rect1 exact-cover kernel, folding the output array
-- with any reducer r equals folding the expected (rect1Expected) output.
--
-- This is the semantic justification for Loom's foldFor / accumDVecFor
-- API: the accumulator never needs to be heap-allocated as a PrimVar;
-- GHC can keep it as an unboxed loop variable.
foldFor-correct :
  ∀ {n} →
  (r : Reducer) →
  (kernel : ExactCoverRect1Kernel n) →
  (env : Env rank1) →
  foldArray1D r
    (runRect1 env (kernelProgram (base (wholeKernel kernel))))
    (outputArr (base (wholeKernel kernel))) ≡
  foldArray1D r
    (rect1Expected kernel env)
    (outputArr (base (wholeKernel kernel)))
foldFor-correct r kernel env =
  foldArray1D-cong r
    (runRect1 env (kernelProgram (base (wholeKernel kernel))))
    (rect1Expected kernel env)
    (outputArr (base (wholeKernel kernel)))
    (runRect1-state kernel env (outputArr (base (wholeKernel kernel))))

-- ─────────────────────────────────────────────────────────────────────────────
-- Corollary for sumReducer
-- ─────────────────────────────────────────────────────────────────────────────

foldFor-sum-correct :
  ∀ {n} →
  (kernel : ExactCoverRect1Kernel n) →
  (env : Env rank1) →
  foldArray1D sumReducer
    (runRect1 env (kernelProgram (base (wholeKernel kernel))))
    (outputArr (base (wholeKernel kernel))) ≡
  foldArray1D sumReducer
    (rect1Expected kernel env)
    (outputArr (base (wholeKernel kernel)))
foldFor-sum-correct = foldFor-correct sumReducer
