{-# OPTIONS --safe #-}

module Loom.Theory.Index where

open import Loom.Theory.Prelude
open import Loom.Theory.Schedule
open import Loom.Theory.Shape

RectIx : ∀ {rank} → Shape rank → Set
RectIx {rank1} (shape1 extent) = Fin extent
RectIx {rank2} (shape2 rowCount colCount) = Fin rowCount × Fin colCount

rowOf : ∀ {shape : Shape rank2} → RectIx shape → Fin (rows shape)
rowOf {shape2 _ _} (row , _) = row

colOf : ∀ {shape : Shape rank2} → RectIx shape → Fin (cols shape)
colOf {shape2 _ _} (_ , col) = col

record TileIx (shape tileShape : Shape rank2) : Set where
  constructor tileIx
  field
    origin : RectIx shape
    local : RectIx tileShape
    rowBound : toℕ (rowOf origin) + toℕ (rowOf local) < rows shape
    colBound : toℕ (colOf origin) + toℕ (colOf local) < cols shape

open TileIx public

global : ∀ {shape tileShape} → TileIx shape tileShape → RectIx shape
global {shape2 _ _} ix = fromℕ< (rowBound ix) , fromℕ< (colBound ix)

Index : ∀ {rank} → Schedule rank → Shape rank → Set
Index rect shape = RectIx shape
Index (tile tileShape) shape = TileIx shape tileShape
