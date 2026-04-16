{-# OPTIONS --safe #-}

module Loom.Theory.Shape where

open import Data.Nat.Properties as ℕₚ using (_≟_)
open import Loom.Theory.Prelude
open import Relation.Nullary using (Dec; no; yes)

data Rank : Set where
  rank1 : Rank
  rank2 : Rank

data Shape : Rank → Set where
  shape1 : (extent : ℕ) → Shape rank1
  shape2 : (rows cols : ℕ) → Shape rank2

extent1 : Shape rank1 → ℕ
extent1 (shape1 extent) = extent

rows : Shape rank2 → ℕ
rows (shape2 rowCount _) = rowCount

cols : Shape rank2 → ℕ
cols (shape2 _ colCount) = colCount

record Array (rank : Rank) : Set where
  constructor array
  field
    ident : ℕ
    shape : Shape rank

open Array public

shapeEq : ∀ {rank} → (left right : Shape rank) → Dec (left ≡ right)
shapeEq {rank1} (shape1 leftExtent) (shape1 rightExtent) with leftExtent ℕₚ.≟ rightExtent
... | yes refl = yes refl
... | no left≢right = no λ where refl → left≢right refl
shapeEq {rank2} (shape2 leftRows leftCols) (shape2 rightRows rightCols) with leftRows ℕₚ.≟ rightRows | leftCols ℕₚ.≟ rightCols
... | yes refl | yes refl = yes refl
... | no left≢right | _ = no λ where refl → left≢right refl
... | _ | no left≢right = no λ where refl → left≢right refl

arrayEq : ∀ {rank} → (left right : Array rank) → Dec (left ≡ right)
arrayEq (array leftIdent leftShape) (array rightIdent rightShape) with leftIdent ℕₚ.≟ rightIdent | shapeEq leftShape rightShape
... | yes refl | yes refl = yes refl
... | no left≢right | _ = no λ where refl → left≢right refl
... | _ | no left≢right = no λ where refl → left≢right refl
