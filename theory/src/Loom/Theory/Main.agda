{-# OPTIONS --safe #-}

module Loom.Theory.Main where

open import Loom.Theory.Access
open import Loom.Theory.Determinism
open import Loom.Theory.Examples
open import Loom.Theory.ExactCoverRect1
open import Loom.Theory.ExactCoverRect2
open import Loom.Theory.ExactCoverTiled using () renaming (wholeKernel to tiledWholeKernel)
open import Loom.Theory.ExactCoverTiled
open import Loom.Theory.Index
open import Loom.Theory.Prelude
open import Loom.Theory.FullRunTheorems
open import Loom.Theory.Pointwise
open import Loom.Theory.ProgramTheorems
open import Loom.Theory.RectExecution
open import Loom.Theory.LoopInterchange
open import Loom.Theory.Phase
open import Loom.Theory.PhaseSemantics
open import Loom.Theory.PhaseTheorems
open import Loom.Theory.Reduction
open import Loom.Theory.ReductionTheorems
open import Loom.Theory.Safety
open import Loom.Theory.SchedulePreservation
open import Loom.Theory.Schedule
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.StripMineTiling
open import Loom.Theory.Syntax
open import Loom.Theory.Traversal
open import Loom.Theory.Traversal.Export
open import Loom.Theory.TiledPointwise
open import Loom.Theory.WholeTiled
open import Loom.Theory.WholeRect1
open import Loom.Theory.WholeRect2

two : ℕ
two = suc (suc zero)

two-plus-two : two + two ≡ suc (suc (suc (suc zero)))
two-plus-two = refl

rect-capability : Capability
rect-capability = ReadOnly

rect-copy-demo : lookupEnv (runAt initial₂ rect-copy zero₂) output zero₂ ≡ 7
rect-copy-demo = rect-copy-zero

tiled-copy-demo : lookupEnv (runAt initial₂ tiled-copy tile-zero) output zero₂ ≡ 7
tiled-copy-demo = tiled-copy-zero

line-copy-demo : lookupEnv (runRect1 line-initial line-rect-copy) line-output line-2 ≡ 4
line-copy-demo = line-output-2

line-inc-demo : lookupEnv (runAt line-initial line-rect-inc line-0) line-inc-output line-0 ≡ 5
line-inc-demo = line-inc-zero

tiny-rect-full-demo : lookupEnv (runRect2 tiny-initial₂ tiny-rect-copy) tiny-output tiny-zero₂ ≡ 5
tiny-rect-full-demo = tiny-rect-copy-full-zero

tiny-rect-full-demo-11 : lookupEnv (runRect2 tiny-initial₂ tiny-rect-copy) tiny-output tiny-11 ≡ 5
tiny-rect-full-demo-11 = tiny-output-11

tiny-inc-full-demo-11 : lookupEnv (runRect2 tiny-initial₂ tiny-rect-inc) tiny-inc-output tiny-11 ≡ 6
tiny-inc-full-demo-11 = tiny-inc-output-11

tiny-copy-loop-interchange-demo :
  lookupEnv (runRect2 tiny-initial₂ tiny-rect-copy) tiny-output tiny-11 ≡
    lookupEnv (runRect2Interchange tiny-initial₂ tiny-rect-copy) tiny-output tiny-11
tiny-copy-loop-interchange-demo = tiny-copy-loop-interchange-11

tiny-stripmine-demo :
  lookupEnv (runRect2 tiny-initial₂ tiny-rect-copy) tiny-output tiny-11 ≡
    lookupEnv (runTiles tiny-initial₂ (tiledWholeKernel tiny-stripmine-exact-kernel)) tiny-output tiny-11
tiny-stripmine-demo = tiny-stripmine-copy-11

line-sum-demo : foldArray1D sumReducer line-initial line-input ≡ 12
line-sum-demo = line-sum-is-12

line-inc-sum-demo : foldArray1D sumReducer (runRect1 line-initial line-rect-inc) line-inc-output ≡ 15
line-inc-sum-demo = line-inc-sum-is-15

line-phased-demo : lookupEnv (runPhases line-initial line-copy-then-inc) line-inc-output line-2 ≡ 5
line-phased-demo = line-phased-output-2
