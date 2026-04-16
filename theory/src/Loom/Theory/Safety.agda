{-# OPTIONS --safe #-}

module Loom.Theory.Safety where

open import Loom.Theory.Access
open import Loom.Theory.Index
open import Loom.Theory.Prelude
open import Loom.Theory.Schedule
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Relation.Nullary using (Dec; no; yes)

shape-safe : ∀ {cap rank sched arr} → Access {rank} cap sched arr → RectIx (shape arr)
shape-safe = resolve

schedule-safe-rect :
  ∀ {cap rank arr} →
  Access {rank} cap rect arr →
  Index rect (shape arr)
schedule-safe-rect access = ix access

schedule-safe-tile :
  ∀ {cap tileShape arr} →
  Access cap (tile tileShape) arr →
  TileIx (shape arr) tileShape
schedule-safe-tile access = ix access

read-after-write-same :
  ∀ {cap rank sched arr} →
  (store : Store (shape arr)) →
  (access : Access {rank} cap sched arr) →
  (value : ℕ) →
  readStore (writeStore store access value) access ≡ value
read-after-write-same store access value = writeRect-self store (resolve access) value
