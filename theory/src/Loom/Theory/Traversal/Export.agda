{-# OPTIONS --safe #-}

module Loom.Theory.Traversal.Export where

open import Data.List.Base using (List; []; _∷_)
open import Loom.Theory.Index
open import Loom.Theory.Prelude
open import Loom.Theory.Shape
open import Loom.Theory.Traversal

data Coord1 : Set where
  coord1 : ℕ → Coord1

data Coord2 : Set where
  coord2 : ℕ → ℕ → Coord2

appendList : ∀ {A : Set} → List A → List A → List A
appendList [] ys = ys
appendList (x ∷ xs) ys = x ∷ appendList xs ys

appendInner :
  ∀ {A : Set} →
  (enumeration : Enumerator A) →
  (outer : Fin (outerCount enumeration)) →
  List A →
  List A
appendInner enumeration outer acc =
  foldFin
    (λ inner acc′ → elemAt enumeration outer inner ∷ acc′)
    acc

enumToList : ∀ {A : Set} → Enumerator A → List A
enumToList enumeration =
  foldFin
    (λ outer acc → appendInner enumeration outer acc)
    []

reverseList : ∀ {A : Set} → List A → List A
reverseList [] = []
reverseList (x ∷ xs) = appendList (reverseList xs) (x ∷ [])

enumeratorToList : ∀ {A : Set} → Enumerator A → List A
enumeratorToList enumeration = reverseList (enumToList enumeration)

coord1Of : ∀ {n} → RectIx (shape1 n) → Coord1
coord1Of ix = coord1 (toℕ ix)

coord2Of : ∀ {rows cols} → RectIx (shape2 rows cols) → Coord2
coord2Of (row , col) = coord2 (toℕ row) (toℕ col)

mapCoord1 : ∀ {n} → List (RectIx (shape1 n)) → List Coord1
mapCoord1 [] = []
mapCoord1 (ix ∷ ixs) = coord1Of ix ∷ mapCoord1 ixs

mapCoord2 : ∀ {rows cols} → List (RectIx (shape2 rows cols)) → List Coord2
mapCoord2 [] = []
mapCoord2 (ix ∷ ixs) = coord2Of ix ∷ mapCoord2 ixs

rect1Coords : ℕ → List Coord1
rect1Coords n = mapCoord1 (enumeratorToList (rect1Traversal {n}))

rowMajorCoords2D : (rows cols : ℕ) → List Coord2
rowMajorCoords2D rows cols =
  mapCoord2 (enumeratorToList (rowMajorTraversal2D {rows} {cols}))

columnMajorCoords2D : (rows cols : ℕ) → List Coord2
columnMajorCoords2D rows cols =
  mapCoord2 (enumeratorToList (columnMajorTraversal2D {rows} {cols}))
