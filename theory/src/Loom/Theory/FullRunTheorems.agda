{-# OPTIONS --safe #-}

module Loom.Theory.FullRunTheorems where

open import Loom.Theory.Examples
open import Loom.Theory.ExactCoverRect1
open import Loom.Theory.ExactCoverRect2
open import Loom.Theory.ExactCoverTiled using () renaming (wholeKernel to tiledWholeKernel)
open import Loom.Theory.LoopInterchange
open import Loom.Theory.PhaseSemantics
open import Loom.Theory.PhaseTheorems
open import Loom.Theory.Prelude
open import Loom.Theory.RectExecution
open import Loom.Theory.StripMineTiling
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.WholeTiled
open import Loom.Theory.WholeRect1
open import Loom.Theory.WholeRect2

line-output-0 :
  lookupEnv (runRect1 line-initial line-rect-copy) line-output line-0 ≡ 4
line-output-0 = runRect1-pointwise line-whole-kernel line-initial line-0

line-output-1 :
  lookupEnv (runRect1 line-initial line-rect-copy) line-output line-1 ≡ 4
line-output-1 = runRect1-pointwise line-whole-kernel line-initial line-1

line-output-2 :
  lookupEnv (runRect1 line-initial line-rect-copy) line-output line-2 ≡ 4
line-output-2 = runRect1-pointwise line-whole-kernel line-initial line-2

line-input-unchanged-0 :
  lookupEnv (runRect1 line-initial line-rect-copy) line-input line-0 ≡ 4
line-input-unchanged-0 = runRect1-input-preserved line-whole-kernel line-initial line-0

tiny-output-00 :
  lookupEnv (runRect2 tiny-initial₂ tiny-rect-copy) tiny-output tiny-00 ≡ 5
tiny-output-00 = runRect2-pointwise tiny-copy-whole-kernel tiny-initial₂ fzero fzero

tiny-output-01 :
  lookupEnv (runRect2 tiny-initial₂ tiny-rect-copy) tiny-output tiny-01 ≡ 5
tiny-output-01 = runRect2-pointwise tiny-copy-whole-kernel tiny-initial₂ fzero (fsuc fzero)

tiny-output-10 :
  lookupEnv (runRect2 tiny-initial₂ tiny-rect-copy) tiny-output tiny-10 ≡ 5
tiny-output-10 = runRect2-pointwise tiny-copy-whole-kernel tiny-initial₂ (fsuc fzero) fzero

tiny-output-11 :
  lookupEnv (runRect2 tiny-initial₂ tiny-rect-copy) tiny-output tiny-11 ≡ 5
tiny-output-11 = runRect2-pointwise tiny-copy-whole-kernel tiny-initial₂ (fsuc fzero) (fsuc fzero)

tiny-input-unchanged-00 :
  lookupEnv (runRect2 tiny-initial₂ tiny-rect-copy) tiny-input tiny-00 ≡ 5
tiny-input-unchanged-00 = runRect2-input-preserved tiny-copy-whole-kernel tiny-initial₂ tiny-00

tiny-inc-output-00 :
  lookupEnv (runRect2 tiny-initial₂ tiny-rect-inc) tiny-inc-output tiny-00 ≡ 6
tiny-inc-output-00 = runRect2-pointwise tiny-inc-whole-kernel tiny-initial₂ fzero fzero

tiny-inc-output-11 :
  lookupEnv (runRect2 tiny-initial₂ tiny-rect-inc) tiny-inc-output tiny-11 ≡ 6
tiny-inc-output-11 = runRect2-pointwise tiny-inc-whole-kernel tiny-initial₂ (fsuc fzero) (fsuc fzero)

line-exact-state-2 :
  lookupEnv (runRect1 line-initial line-rect-copy) line-output line-2 ≡
    rect1Expected line-exact-kernel line-initial line-output line-2
line-exact-state-2 = runRect1-state line-exact-kernel line-initial line-output line-2

tiny-copy-exact-state-11 :
  lookupEnv (runRect2 tiny-initial₂ tiny-rect-copy) tiny-output tiny-11 ≡
    rect2Expected tiny-copy-exact-kernel tiny-initial₂ tiny-output tiny-11
tiny-copy-exact-state-11 = runRect2-state tiny-copy-exact-kernel tiny-initial₂ tiny-output tiny-11

tiny-copy-loop-interchange-11 :
  lookupEnv (runRect2 tiny-initial₂ tiny-rect-copy) tiny-output tiny-11 ≡
    lookupEnv (runRect2Interchange tiny-initial₂ tiny-rect-copy) tiny-output tiny-11
tiny-copy-loop-interchange-11 =
  loop-interchange-preserves-state
    tiny-copy-exact-kernel
    tiny-initial₂
    tiny-output
    tiny-11

tiny-stripmine-copy-11 :
  lookupEnv (runRect2 tiny-initial₂ tiny-rect-copy) tiny-output tiny-11 ≡
    lookupEnv (runTiles tiny-initial₂ (tiledWholeKernel tiny-stripmine-exact-kernel)) tiny-output tiny-11
tiny-stripmine-copy-11 =
  tiny-stripmine-preserves-state
    tiny-output
    tiny-11

line-phased-output-2 :
  lookupEnv (runPhases line-initial line-copy-then-inc) line-inc-output line-2 ≡ 5
line-phased-output-2 =
  trans
    (runRect1-pointwise line-output-inc-whole-kernel (runRect1 line-initial line-rect-copy) line-2)
    (cong suc line-output-2)

line-copy-then-inc-cong :
  (left right : Env rank1) →
  PostStateEq left right →
  PostStateEq
    (runPhases left line-copy-then-inc)
    (runPhases right line-copy-then-inc)
line-copy-then-inc-cong left right envEq =
  runPhases-cong left right envEq line-copy-then-inc
