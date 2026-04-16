{-# OPTIONS --safe #-}

module Loom.Theory.ExactCoverTiled where

open import Loom.Theory.Access
open import Loom.Theory.Index
open import Loom.Theory.Pointwise
open import Loom.Theory.Prelude
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.TiledPointwise
open import Loom.Theory.WholeTiled
open import Relation.Nullary using (no; yes)

record ExactCoverTiledKernel (n : ℕ) (dom tileShape : Shape rank2) : Set where
  field
    wholeKernel : WholeTiledKernel n dom tileShape
    coverIndex :
      RectIx (shape (outputArr (baseKernel wholeKernel))) → Fin n
    outputCovered :
      (target : RectIx (shape (outputArr (baseKernel wholeKernel)))) →
      outputGlobal (baseKernel wholeKernel) (tileAt wholeKernel (coverIndex target)) ≡ target

open ExactCoverTiledKernel public

runTiles-covered-pointwise :
  ∀ {n dom tileShape} →
  (kernel : ExactCoverTiledKernel n dom tileShape) →
  (env : Env rank2) →
  (target : RectIx (shape (outputArr (baseKernel (wholeKernel kernel))))) →
  lookupEnv (runTiles env (wholeKernel kernel)) (outputArr (baseKernel (wholeKernel kernel))) target ≡
    transform (baseKernel (wholeKernel kernel))
      (lookupEnv env (inputArr (baseKernel (wholeKernel kernel)))
        (inputGlobal
          (baseKernel (wholeKernel kernel))
          (tileAt (wholeKernel kernel) (coverIndex kernel target))))
runTiles-covered-pointwise kernel env target =
  trans
    (cong
      (lookupEnv (runTiles env (wholeKernel kernel)) (outputArr (baseKernel (wholeKernel kernel))))
      (sym (outputCovered kernel target)))
    (runTiles-pointwise (wholeKernel kernel) env (coverIndex kernel target))

runTiles-covered-input-preserved :
  ∀ {n dom tileShape} →
  (kernel : ExactCoverTiledKernel n dom tileShape) →
  (env : Env rank2) →
  (j : RectIx (shape (inputArr (baseKernel (wholeKernel kernel))))) →
  lookupEnv (runTiles env (wholeKernel kernel)) (inputArr (baseKernel (wholeKernel kernel))) j ≡
    lookupEnv env (inputArr (baseKernel (wholeKernel kernel))) j
runTiles-covered-input-preserved kernel env j =
  runTiles-input-preserved (wholeKernel kernel) env j

tiledExpected :
  ∀ {n dom tileShape} →
  (kernel : ExactCoverTiledKernel n dom tileShape) →
  (env : Env rank2) →
  (arr : Array rank2) →
  RectIx (shape arr) →
  ℕ
tiledExpected kernel env arr ix with arrayEq arr (outputArr (baseKernel (wholeKernel kernel)))
... | yes refl =
  transform (baseKernel (wholeKernel kernel))
    (lookupEnv env (inputArr (baseKernel (wholeKernel kernel)))
      (inputGlobal
        (baseKernel (wholeKernel kernel))
        (tileAt (wholeKernel kernel) (coverIndex kernel ix))))
... | no _ = lookupEnv env arr ix

runTiles-state :
  ∀ {n dom tileShape} →
  (kernel : ExactCoverTiledKernel n dom tileShape) →
  (env : Env rank2) →
  (arr : Array rank2) →
  (ix : RectIx (shape arr)) →
  lookupEnv (runTiles env (wholeKernel kernel)) arr ix ≡
    tiledExpected kernel env arr ix
runTiles-state kernel env arr ix with arrayEq arr (outputArr (baseKernel (wholeKernel kernel)))
runTiles-state kernel env arr ix | yes refl = runTiles-covered-pointwise kernel env ix
runTiles-state kernel env arr ix | no arr≢output =
  runTiles-unrelated
    (wholeKernel kernel)
    env
    arr
    (λ eq → arr≢output (sym eq))
    ix

runTiles-state-eq :
  ∀ {n dom tileShape} →
  (kernel : ExactCoverTiledKernel n dom tileShape) →
  (env : Env rank2) →
  PostStateEq
    (runTiles env (wholeKernel kernel))
    (tiledExpected kernel env)
runTiles-state-eq kernel env arr ix = runTiles-state kernel env arr ix
