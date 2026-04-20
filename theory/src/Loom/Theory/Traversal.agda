{-# OPTIONS --safe #-}

module Loom.Theory.Traversal where

open import Data.Fin.Base using (_↑ˡ_; _↑ʳ_; combine; remQuot)
open import Data.Fin.Properties using (remQuot-combine; combine-remQuot)
open import Data.Nat.Base using (_*_) renaming (_+_ to _+ℕ_)
open import Data.Product.Base using (map₁)
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

-- remQuot of a left-injection is (fzero , i)
remQuot-↑ˡ : ∀ {m n : ℕ} (i : Fin n) → remQuot n (i ↑ˡ (m * n)) ≡ (fzero , i)
remQuot-↑ˡ {m} {n} i = remQuot-combine {n = suc m} {k = n} fzero i

-- remQuot of a right-injection bumps the quotient by one
remQuot-↑ʳ : ∀ {m : ℕ} (n : ℕ) (x : Fin (m * n)) → remQuot n (n ↑ʳ x) ≡ map₁ fsuc (remQuot n x)
remQuot-↑ʳ {m} n x =
  trans
    (cong (λ y → remQuot n (n ↑ʳ y)) (sym (combine-remQuot {n = m} n x)))
    (remQuot-combine {n = suc m} {k = n} (fsuc (proj₁ (remQuot {m} n x))) (proj₂ (remQuot {m} n x)))

-- foldFin respects pointwise equality of bodies
foldFin-cong :
  ∀ {A : Set} {n : ℕ} {f g : Fin n → A → A} →
  (∀ i acc → f i acc ≡ g i acc) →
  (init : A) →
  foldFin f init ≡ foldFin g init
foldFin-cong {n = zero}  h init = refl
foldFin-cong {n = suc n} {f = f} {g = g} h init =
  trans
    (foldFin-cong (λ i acc → h (fsuc i) acc) (f fzero init))
    (cong (foldFin (λ i acc → g (fsuc i) acc)) (h fzero init))

-- foldFin over (outer + inner) splits into inner-then-outer passes
-- Left half: indices i ↑ˡ inner  (first outer steps)
-- Right half: indices outer ↑ʳ j (last inner steps)
foldFin-append :
  ∀ {A : Set} {outer inner : ℕ} →
  (body : Fin (outer +ℕ inner) → A → A) →
  (init : A) →
  foldFin body init ≡
  foldFin (λ i → body (outer ↑ʳ i)) (foldFin (λ j → body (j ↑ˡ inner)) init)
foldFin-append {outer = zero}       body init = refl
foldFin-append {outer = suc outer'} {inner} body init =
  foldFin-append {outer = outer'} {inner = inner} (λ i → body (fsuc i)) (body fzero init)

-- Nested foldFin over (rows × cols) equals flat foldFin via remQuot
foldFin-product :
  ∀ {A : Set} {rows cols : ℕ} →
  (f : Fin rows → Fin cols → A → A) →
  (init : A) →
  foldFin (λ i acc → foldFin (f i) acc) init ≡
  foldFin (λ flat acc → let (i , j) = remQuot cols flat in f i j acc) init
foldFin-product {rows = zero}         f init = refl
foldFin-product {rows = suc rows'} {cols} f init =
  trans
    (foldFin-product (λ i → f (fsuc i)) (foldFin (f fzero) init))
    (sym
      (trans
        (foldFin-append {outer = cols} {inner = rows' * cols}
          (λ flat acc → let (i , j) = remQuot cols flat in f i j acc)
          init)
        (trans
          (cong
            (foldFin (λ x acc → let (i , j) = remQuot cols (cols ↑ʳ x) in f i j acc))
            (foldFin-cong
              (λ j acc → cong (λ p → f (proj₁ p) (proj₂ p) acc) (remQuot-↑ˡ {m = rows'} j))
              init))
          (foldFin-cong
            (λ x acc → cong (λ p → f (proj₁ p) (proj₂ p) acc) (remQuot-↑ʳ {m = rows'} cols x))
            (foldFin (f fzero) init)))))
