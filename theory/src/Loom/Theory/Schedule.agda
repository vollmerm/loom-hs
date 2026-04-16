{-# OPTIONS --safe #-}

module Loom.Theory.Schedule where

open import Loom.Theory.Shape

data Schedule : Rank → Set where
  rect : ∀ {rank} → Schedule rank
  tile : Shape rank2 → Schedule rank2
