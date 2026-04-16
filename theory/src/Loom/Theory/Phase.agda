{-# OPTIONS --safe #-}

module Loom.Theory.Phase where

open import Loom.Theory.Prelude
open import Loom.Theory.Schedule
open import Loom.Theory.Shape
open import Loom.Theory.Syntax

infixr 5 _>>>_

data PhasedProgram (rank : Rank) : Set where
  done : PhasedProgram rank
  step : ∀ {shape : Shape rank} → Program rect shape → PhasedProgram rank → PhasedProgram rank

singlePhase : ∀ {rank} {shape : Shape rank} → Program rect shape → PhasedProgram rank
singlePhase prog = step prog done

_>>>_ : ∀ {rank} {shape : Shape rank} → Program rect shape → PhasedProgram rank → PhasedProgram rank
_>>>_ = step
