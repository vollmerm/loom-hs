{-# OPTIONS --safe #-}

module Loom.Theory.ProgramTheorems where

open import Loom.Theory.Access
open import Loom.Theory.Examples
open import Loom.Theory.Index
open import Loom.Theory.Pointwise
open import Loom.Theory.Prelude
open import Loom.Theory.Schedule
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.TiledPointwise

line-copy-correct :
  (env : Env rank1) →
  (ix : RectIx line) →
  lookupEnv (runAt env line-rect-copy ix) line-output ix ≡ lookupEnv env line-input ix
line-copy-correct env ix = runAt-pointwise line-copy-kernel env ix

line-input-preserved :
  (env : Env rank1) →
  (ix : RectIx line) →
  (j : RectIx line) →
  lookupEnv (runAt env line-rect-copy ix) line-input j ≡ lookupEnv env line-input j
line-input-preserved env ix j = runAt-input-preserved line-copy-kernel env ix j

rect-copy-correct :
  (env : Env rank2) →
  (ix : RectIx board) →
  lookupEnv (runAt env rect-copy ix) output ix ≡ lookupEnv env input ix
rect-copy-correct env ix = runAt-pointwise rect-copy-kernel env ix

rect-copy-input-preserved :
  (env : Env rank2) →
  (ix : RectIx board) →
  (j : RectIx board) →
  lookupEnv (runAt env rect-copy ix) input j ≡ lookupEnv env input j
rect-copy-input-preserved env ix j = runAt-input-preserved rect-copy-kernel env ix j

tiled-copy-correct :
  (env : Env rank2) →
  (ix : TileIx board tile4x4) →
  lookupEnv (runAt env tiled-copy ix) output (global ix) ≡ lookupEnv env input (global ix)
tiled-copy-correct env ix = runAt-tiled-pointwise tiled-copy-global-kernel env ix

tiled-copy-input-preserved :
  (env : Env rank2) →
  (ix : TileIx board tile4x4) →
  (j : RectIx board) →
  lookupEnv (runAt env tiled-copy ix) input j ≡ lookupEnv env input j
tiled-copy-input-preserved env ix j = runAt-tiled-input-preserved tiled-copy-global-kernel env ix j

rect-inc-correct :
  (env : Env rank2) →
  (ix : RectIx board) →
  lookupEnv (runAt env rect-inc ix) output ix ≡ suc (lookupEnv env input ix)
rect-inc-correct env ix = runAt-pointwise rect-inc-kernel env ix
