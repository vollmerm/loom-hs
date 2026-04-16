{-# OPTIONS --safe #-}

module Loom.Theory.TiledPointwise where

open import Loom.Theory.Access
open import Loom.Theory.Index
open import Loom.Theory.Pointwise
open import Loom.Theory.Prelude
open import Loom.Theory.Schedule
open import Loom.Theory.Semantics
open import Loom.Theory.Shape

record TiledPointwiseKernel (dom tileShape : Shape rank2) : Set where
  field
    inputArr : Array rank2
    outputArr : Array rank2
    distinct : inputArr ≢ outputArr
    input-shape : shape inputArr ≡ dom
    output-shape : shape outputArr ≡ dom
    inputAt : TileIx dom tileShape → Access ReadOnly (tile tileShape) inputArr
    outputAt : TileIx dom tileShape → Access WriteOnly (tile tileShape) outputArr
    transform : ℕ → ℕ
    input-global :
      (ix : TileIx dom tileShape) →
      resolve (inputAt ix) ≡ subst RectIx (sym input-shape) (global ix)
    output-global :
      (ix : TileIx dom tileShape) →
      resolve (outputAt ix) ≡ subst RectIx (sym output-shape) (global ix)

open TiledPointwiseKernel public

base :
  ∀ {dom tileShape} →
  TiledPointwiseKernel dom tileShape →
  PointwiseKernel (tile tileShape) dom
base kernel = record
  { inputArr = inputArr kernel
  ; outputArr = outputArr kernel
  ; distinct = distinct kernel
  ; inputAt = inputAt kernel
  ; outputAt = outputAt kernel
  ; transform = transform kernel
  }

inputGlobal :
  ∀ {dom tileShape} →
  (kernel : TiledPointwiseKernel dom tileShape) →
  TileIx dom tileShape →
  RectIx (shape (inputArr kernel))
inputGlobal kernel ix = subst RectIx (sym (input-shape kernel)) (global ix)

outputGlobal :
  ∀ {dom tileShape} →
  (kernel : TiledPointwiseKernel dom tileShape) →
  TileIx dom tileShape →
  RectIx (shape (outputArr kernel))
outputGlobal kernel ix = subst RectIx (sym (output-shape kernel)) (global ix)

runAt-tiled-pointwise :
  ∀ {dom tileShape} →
  (kernel : TiledPointwiseKernel dom tileShape) →
  (env : Env rank2) →
  (ix : TileIx dom tileShape) →
  lookupEnv (runAt env (kernelProgram (base kernel)) ix) (outputArr kernel) (outputGlobal kernel ix) ≡
    transform kernel (lookupEnv env (inputArr kernel) (inputGlobal kernel ix))
runAt-tiled-pointwise kernel env ix =
  trans
    (cong
      (lookupEnv (runAt env (kernelProgram (base kernel)) ix) (outputArr kernel))
      (sym (output-global kernel ix)))
    (trans
      (runAt-pointwise (base kernel) env ix)
      (cong
        (λ j → transform kernel (lookupEnv env (inputArr kernel) j))
        (input-global kernel ix)))

runAt-tiled-unrelated :
  ∀ {dom tileShape} →
  (kernel : TiledPointwiseKernel dom tileShape) →
  (env : Env rank2) →
  (ix : TileIx dom tileShape) →
  (other : Array rank2) →
  outputArr kernel ≢ other →
  (j : RectIx (shape other)) →
  lookupEnv (runAt env (kernelProgram (base kernel)) ix) other j ≡ lookupEnv env other j
runAt-tiled-unrelated kernel env ix other output≢other j =
  runAt-unrelated (base kernel) env ix other output≢other j

runAt-tiled-input-preserved :
  ∀ {dom tileShape} →
  (kernel : TiledPointwiseKernel dom tileShape) →
  (env : Env rank2) →
  (ix : TileIx dom tileShape) →
  (j : RectIx (shape (inputArr kernel))) →
  lookupEnv (runAt env (kernelProgram (base kernel)) ix) (inputArr kernel) j ≡
    lookupEnv env (inputArr kernel) j
runAt-tiled-input-preserved kernel env ix j =
  runAt-input-preserved (base kernel) env ix j
