{-# OPTIONS --safe #-}

module Loom.Theory.Traversal where

open import Loom.Theory.Index
open import Loom.Theory.Prelude
open import Loom.Theory.Schedule
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.Syntax

foldFin : ∀ {n : ℕ} {A : Set} → (Fin n → A → A) → A → A
foldFin {zero} body acc = acc
foldFin {suc n} body acc = foldFin (λ ix → body (fsuc ix)) (body fzero acc)

record Enumerator (A : Set) : Set where
  constructor enumerator
  field
    outerCount : ℕ
    innerCount : Fin outerCount → ℕ
    elemAt : (i : Fin outerCount) → Fin (innerCount i) → A

open Enumerator public

record LinearTraversal {A : Set} (n : ℕ) : Set where
  constructor linearTraversal
  field
    stepAt : Fin n → A

  traversal : Enumerator A
  traversal = enumerator n (λ _ → suc zero) (λ i _ → stepAt i)

open LinearTraversal public

runEnumerator :
  ∀ {rank sched shape} →
  Env rank →
  Program {rank} sched shape →
  Enumerator (Index sched shape) →
  Env rank
runEnumerator env prog enumeration =
  foldFin
    (λ outer env′ →
      foldFin
        (λ inner env″ → runAt env″ prog (elemAt enumeration outer inner))
        env′)
    env
  where
    open Enumerator enumeration

rect1Traversal :
  ∀ {n} →
  Enumerator (RectIx (shape1 n))
rect1Traversal {n} = traversal (linearTraversal (λ i → i))

rowMajorTraversal2D :
  ∀ {rows cols} →
  Enumerator (RectIx (shape2 rows cols))
rowMajorTraversal2D {rows} {cols} =
  enumerator
    rows
    (λ _ → cols)
    (λ row col → row , col)

columnMajorTraversal2D :
  ∀ {rows cols} →
  Enumerator (RectIx (shape2 rows cols))
columnMajorTraversal2D {rows} {cols} =
  enumerator
    cols
    (λ _ → rows)
    (λ col row → row , col)

explicitTileTraversal2D :
  ∀ {n dom tileShape} →
  (Fin n → TileIx dom tileShape) →
  Enumerator (TileIx dom tileShape)
explicitTileTraversal2D steps = traversal (linearTraversal steps)

linear-covers :
  ∀ {A n} →
  (steps : LinearTraversal {A} n) →
  (i : Fin n) →
  elemAt (traversal steps) i fzero ≡ stepAt steps i
linear-covers steps i = refl

rect1-covers :
  ∀ {n} →
  (ix : RectIx (shape1 n)) →
  elemAt rect1Traversal ix fzero ≡ ix
rect1-covers ix = refl

rowMajor-covers :
  ∀ {rows cols} →
  (ix : RectIx (shape2 rows cols)) →
  elemAt rowMajorTraversal2D (rowOf ix) (colOf ix) ≡ ix
rowMajor-covers ix = refl

columnMajor-covers :
  ∀ {rows cols} →
  (ix : RectIx (shape2 rows cols)) →
  elemAt columnMajorTraversal2D (colOf ix) (rowOf ix) ≡ ix
columnMajor-covers ix = refl
