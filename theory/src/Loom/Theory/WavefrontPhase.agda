{-# OPTIONS --safe #-}

-- Wavefront phased execution for diagonal-dependent 2D kernels.
--
-- KEY THEOREM  (wavefront-is-enumerator)
--
--   The "phase-by-phase" execution of a kernel over an anti-diagonal
--   wavefront is definitionally equal to running the wavefront
--   enumerator directly:
--
--     runWavefrontPhases env prog ≡ runEnumerator env prog (wavefrontEnum r c)
--
--   The proof is refl — both sides unfold to the same nested foldFin.

module Loom.Theory.WavefrontPhase where

open import Loom.Theory.Index
open import Loom.Theory.Prelude
open import Loom.Theory.Schedule
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.Syntax
open import Loom.Theory.Traversal
open import Loom.Theory.WavefrontTraversal

-- Execute a 2D kernel by iterating over anti-diagonals (outer) and
-- positions within each diagonal (inner).  This is the explicit
-- "phased" form that matches how wavefront schedules are usually
-- described: process all cells at diagonal-sum d before moving to d+1.
runWavefrontPhases :
  ∀ {r c} →
  Env rank2 →
  Program {rank2} rect (shape2 (suc r) (suc c)) →
  Env rank2
runWavefrontPhases {r} {c} env prog =
  foldFin
    (λ d env′ →
      foldFin
        (λ pos env″ → runAt env″ prog (elemAt (wavefrontEnum r c) d pos))
        env′)
    env

-- The phased execution is definitionally equal to running the wavefront
-- enumerator, so the proof is refl.
wavefront-is-enumerator :
  ∀ {r c} →
  (env  : Env rank2) →
  (prog : Program {rank2} rect (shape2 (suc r) (suc c))) →
  runWavefrontPhases env prog ≡ runEnumerator env prog (wavefrontEnum r c)
wavefront-is-enumerator env prog = refl
