{-# OPTIONS --safe #-}

module Loom.Theory.Access where

open import Loom.Theory.Index
open import Loom.Theory.Prelude
open import Loom.Theory.Schedule
open import Loom.Theory.Shape

data Capability : Set where
  ReadOnly : Capability
  WriteOnly : Capability
  ReadWrite : Capability

data CanRead : Capability → Set where
  read-only : CanRead ReadOnly
  read-write : CanRead ReadWrite

data CanWrite : Capability → Set where
  write-only : CanWrite WriteOnly
  write-write : CanWrite ReadWrite

record Access {rank : Rank} (cap : Capability) (sched : Schedule rank) (arr : Array rank) : Set where
  constructor accessAt
  field
    ix : Index sched (shape arr)

open Access public

rectAccess : ∀ {cap rank} (arr : Array rank) → Index rect (shape arr) → Access cap rect arr
rectAccess _ = accessAt

tileAccess :
  ∀ {cap tileShape} →
  (arr : Array rank2) →
  TileIx (shape arr) tileShape →
  Access cap (tile tileShape) arr
tileAccess _ = accessAt
