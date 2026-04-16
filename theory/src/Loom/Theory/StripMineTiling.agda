{-# OPTIONS --safe #-}

module Loom.Theory.StripMineTiling where

open import Loom.Theory.Access
open import Loom.Theory.ExactCoverRect2
open import Loom.Theory.ExactCoverTiled
open import Loom.Theory.Examples
open import Loom.Theory.Pointwise
open import Loom.Theory.Prelude
open import Loom.Theory.RectExecution
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.WholeTiled
open import Loom.Theory.WholeRect2
open import Relation.Nullary using (no; yes)

tiny-stripmine-preserves-state :
  PostStateEq
    (runRect2 tiny-initial₂ tiny-rect-copy)
    (runTiles tiny-initial₂ (wholeKernel tiny-stripmine-exact-kernel))
tiny-stripmine-preserves-state arr ix with arrayEq arr tiny-output
tiny-stripmine-preserves-state arr ix | yes refl =
  trans
    (runRect2-covered-pointwise tiny-copy-exact-kernel tiny-initial₂ ix)
    (sym (runTiles-covered-pointwise tiny-stripmine-exact-kernel tiny-initial₂ ix))
tiny-stripmine-preserves-state arr ix | no arr≢output =
  trans
    (runRect2-unrelated
      (wholeKernel tiny-copy-exact-kernel)
      tiny-initial₂
      arr
      (λ eq → arr≢output (sym eq))
      ix)
    (sym
      (runTiles-unrelated
        (wholeKernel tiny-stripmine-exact-kernel)
        tiny-initial₂
        arr
        (λ eq → arr≢output (sym eq))
        ix))
