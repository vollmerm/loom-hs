{-# OPTIONS --safe #-}

module Loom.Theory.Syntax where

open import Loom.Theory.Access
open import Loom.Theory.Index
open import Loom.Theory.Prelude
open import Loom.Theory.Schedule
open import Loom.Theory.Shape

data Ty : Set where
  unit : Ty
  nat : Ty

⟦_⟧Ty : Ty → Set
⟦ unit ⟧Ty = ⊤
⟦ nat ⟧Ty = ℕ

infixl 1 _>>=_

data Kernel {rank : Rank} (sched : Schedule rank) : Ty → Set where
  pure : ∀ {ty} → ⟦ ty ⟧Ty → Kernel sched ty
  _>>=_ : ∀ {ty₁ ty₂} → Kernel sched ty₁ → (⟦ ty₁ ⟧Ty → Kernel sched ty₂) → Kernel sched ty₂
  read : ∀ {cap ty arr} → CanRead cap → Access cap sched arr → (ℕ → Kernel sched ty) → Kernel sched ty
  write : ∀ {cap arr} → CanWrite cap → Access cap sched arr → ℕ → Kernel sched unit

record Program {rank : Rank} (sched : Schedule rank) (shape : Shape rank) : Set where
  constructor program
  field
    body : Index sched shape → Kernel sched unit

open Program public
