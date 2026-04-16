{-# OPTIONS --safe #-}

module Loom.Theory.PhaseSemantics where

open import Loom.Theory.Phase
open import Loom.Theory.Prelude
open import Loom.Theory.RectExecution
open import Loom.Theory.Schedule
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.Syntax

runRectProgram :
  ∀ {rank} {shape : Shape rank} →
  Env rank →
  Program rect shape →
  Env rank
runRectProgram {rank1} {shape1 _} env prog = runRect1 env prog
runRectProgram {rank2} {shape2 _ _} env prog = runRect2 env prog

runPhases : ∀ {rank} → Env rank → PhasedProgram rank → Env rank
runPhases env done = env
runPhases env (step prog phases) = runPhases (runRectProgram env prog) phases

runPhases-single :
  ∀ {rank} {shape : Shape rank} →
  (env : Env rank) →
  (prog : Program rect shape) →
  runPhases env (singlePhase prog) ≡ runRectProgram env prog
runPhases-single env prog = refl

runPhases-step :
  ∀ {rank} {shape : Shape rank} →
  (env : Env rank) →
  (prog : Program rect shape) →
  (phases : PhasedProgram rank) →
  runPhases env (step prog phases) ≡ runPhases (runRectProgram env prog) phases
runPhases-step env prog phases = refl
