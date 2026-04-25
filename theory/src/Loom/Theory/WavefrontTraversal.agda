{-# OPTIONS --safe #-}

-- Wavefront (anti-diagonal) traversal for 2D grids.
--
-- The wavefront enumerator visits cells (row, col) in order of
-- increasing diagonal sum  d = toℕ row + toℕ col.
-- Within each diagonal d the cells are visited in row-ascending order.
--
-- KEY THEOREM  (wavefront-covers)
--
--   For every (row, col) in an (suc r × suc c) grid there exist a
--   diagonal index d and a position pos such that
--
--     elemAt (wavefrontEnum r c) d pos ≡ (row , col)
--
--   Proof: exhibit d = fromℕ< (coverDiag-bound row col)  and
--          pos = fromℕ< (coverPos-bound row col)  (with a cast for the
--          dependent inner-count type), then show the row/column
--          components match via toℕ-injective + arithmetic.

module Loom.Theory.WavefrontTraversal where

open import Data.Fin.Base using (cast)
open import Data.Fin.Properties
  using ( toℕ-injective; toℕ-fromℕ<; fromℕ<-toℕ; toℕ<n; toℕ-cast )
open import Data.Nat.Base using (_∸_) renaming (_⊓_ to _⊓ℕ_)
open import Data.Nat.Properties
  using ( ≤-trans; ≤-pred; ≤-reflexive
        ; m≤m+n
        ; m∸n≤m
        ; ∸-monoʳ-≤
        ; m+n∸n≡m; m+n∸m≡n; m+[n∸m]≡n
        ; m∸[m∸n]≡n
        ; 0∸n≡0
        ; ⊓-glb; m⊓n≤m
        ; +-mono-≤; n≮0; ≤-total
        )
open import Data.Sum.Base using (inj₁; inj₂)
open import Relation.Binary.PropositionalEquality using (cong₂)
open import Loom.Theory.Index
open import Loom.Theory.Prelude
open import Loom.Theory.Schedule
open import Loom.Theory.Shape
open import Loom.Theory.Traversal

-- ─────────────────────────────────────────────────────────────────────────────
-- Arithmetic helpers
-- ─────────────────────────────────────────────────────────────────────────────

-- x < suc a ∸ b  →  b + x ≤ a
lt-add-∸-≤ : ∀ (a b x : ℕ) → x < suc a ∸ b → b + x ≤ a
lt-add-∸-≤ a       zero    x h = ≤-pred h
lt-add-∸-≤ zero    (suc b) x h = ⊥-elim (n≮0 (subst (x <_) (0∸n≡0 b) h))
lt-add-∸-≤ (suc a) (suc b) x h = s≤s (lt-add-∸-≤ a b x h)

-- b + x ≤ a  →  x < suc a ∸ b
add-≤-lt : ∀ (a b x : ℕ) → b + x ≤ a → x < suc a ∸ b
add-≤-lt a       zero    x h = s≤s h
add-≤-lt zero    (suc b) x h = ⊥-elim (n≮0 h)
add-≤-lt (suc a) (suc b) x h = add-≤-lt a b x (≤-pred h)

-- n ∸ (n ∸ m) ≤ m
n∸[n∸m]≤m : ∀ (m n : ℕ) → n ∸ (n ∸ m) ≤ m
n∸[n∸m]≤m m n with ≤-total m n
... | inj₁ m≤n = ≤-reflexive (m∸[m∸n]≡n m≤n)
... | inj₂ n≤m = ≤-trans (m∸n≤m n (n ∸ m)) n≤m

-- (row + col) ∸ c ≤ row   since col : Fin (suc c) means toℕ col ≤ c
iMin≤toℕrow :
  ∀ {r c} (row : Fin (suc r)) (col : Fin (suc c)) →
  (toℕ row + toℕ col) ∸ c ≤ toℕ row
iMin≤toℕrow {r} {c} row col =
  ≤-trans
    (∸-monoʳ-≤ (toℕ row + toℕ col) (≤-pred (toℕ<n col)))
    (≤-reflexive (m+n∸n≡m (toℕ row) (toℕ col)))

-- ─────────────────────────────────────────────────────────────────────────────
-- Diagonal-index arithmetic
-- ─────────────────────────────────────────────────────────────────────────────

-- Minimum row index on diagonal dN in a grid with max column index c
iMin : ℕ → ℕ → ℕ
iMin dN c = dN ∸ c

-- Maximum row index on diagonal dN in a grid with max row index r
iMax : ℕ → ℕ → ℕ
iMax dN r = r ⊓ℕ dN

-- Number of cells on diagonal dN in a grid with max indices (r, c)
cellCount : ℕ → ℕ → ℕ → ℕ
cellCount dN r c = suc (iMax dN r) ∸ iMin dN c

-- Total number of anti-diagonals in an (suc r × suc c) grid
diagCount : ℕ → ℕ → ℕ
diagCount r c = suc (r + c)

-- ─────────────────────────────────────────────────────────────────────────────
-- Bound proofs for the element function
-- ─────────────────────────────────────────────────────────────────────────────

-- The row component of the element lies in [0, r]
wavefront-rowBound :
  ∀ {r c} (d : Fin (diagCount r c)) (pos : Fin (cellCount (toℕ d) r c)) →
  iMin (toℕ d) c + toℕ pos < suc r
wavefront-rowBound {r} {c} d pos =
  s≤s (≤-trans
        (lt-add-∸-≤ (iMax (toℕ d) r) (iMin (toℕ d) c) (toℕ pos) (toℕ<n pos))
        (m⊓n≤m r (toℕ d)))

-- The col component of the element lies in [0, c]
wavefront-colBound :
  ∀ {r c} (d : Fin (diagCount r c)) (pos : Fin (cellCount (toℕ d) r c)) →
  toℕ d ∸ (iMin (toℕ d) c + toℕ pos) < suc c
wavefront-colBound {r} {c} d pos =
  s≤s (≤-trans
        (∸-monoʳ-≤ (toℕ d) (m≤m+n (iMin (toℕ d) c) (toℕ pos)))
        (n∸[n∸m]≤m c (toℕ d)))

-- ─────────────────────────────────────────────────────────────────────────────
-- Wavefront enumerator
-- ─────────────────────────────────────────────────────────────────────────────

-- The anti-diagonal enumerator for an (suc r × suc c) grid.
-- Outer index: diagonal d ∈ [0, r+c].
-- Inner index: position within diagonal d.
-- Element: (row, col) with row = iMin(d,c) + pos,  col = d - row.
wavefrontEnum : ∀ (r c : ℕ) → Enumerator (RectIx (shape2 (suc r) (suc c)))
wavefrontEnum r c =
  enumerator
    (diagCount r c)
    (λ d → cellCount (toℕ d) r c)
    (λ d pos →
      let rowN = iMin (toℕ d) c + toℕ pos
          colN = toℕ d ∸ rowN
      in  fromℕ< (wavefront-rowBound d pos) ,
          fromℕ< (wavefront-colBound d pos))

-- ─────────────────────────────────────────────────────────────────────────────
-- Covering witnesses
-- ─────────────────────────────────────────────────────────────────────────────

-- Every (row, col) lies on diagonal (toℕ row + toℕ col)
coverDiag-bound :
  ∀ {r c} (row : Fin (suc r)) (col : Fin (suc c)) →
  toℕ row + toℕ col < diagCount r c
coverDiag-bound {r} {c} row col =
  s≤s (+-mono-≤ (≤-pred (toℕ<n row)) (≤-pred (toℕ<n col)))

coverDiag :
  ∀ {r c} → RectIx (shape2 (suc r) (suc c)) → Fin (diagCount r c)
coverDiag (row , col) = fromℕ< (coverDiag-bound row col)

-- Within its diagonal, (row, col) is at position toℕ row ∸ iMin dN c
coverPos-bound :
  ∀ {r c} (row : Fin (suc r)) (col : Fin (suc c)) →
  toℕ row ∸ iMin (toℕ row + toℕ col) c < cellCount (toℕ row + toℕ col) r c
coverPos-bound {r} {c} row col =
  add-≤-lt (iMax dN r) k (toℕ row ∸ k)
    (subst (_≤ iMax dN r) (sym step1) rN≤iMax)
  where
    dN      = toℕ row + toℕ col
    k       = iMin dN c
    k≤rN    = iMin≤toℕrow row col
    step1   = m+[n∸m]≡n k≤rN        -- k + (toℕ row ∸ k) ≡ toℕ row
    rN≤iMax = ⊓-glb (≤-pred (toℕ<n row)) (m≤m+n (toℕ row) (toℕ col))
    -- rN≤iMax : toℕ row ≤ r ⊓ℕ dN  (= iMax dN r)

-- Cast coverPos from Fin(cellCount dN r c) to Fin(innerCount(wavefrontEnum)(coverDiag ix))
-- by rewriting the type via toℕ-fromℕ<.
coverPos :
  ∀ {r c} (ix : RectIx (shape2 (suc r) (suc c))) →
  Fin (innerCount (wavefrontEnum r c) (coverDiag ix))
coverPos {r} {c} (row , col) =
  cast
    (sym (cong (λ m → cellCount m r c) (toℕ-fromℕ< (coverDiag-bound row col))))
    (fromℕ< (coverPos-bound row col))

-- ─────────────────────────────────────────────────────────────────────────────
-- Main theorem: wavefront covers every cell
-- ─────────────────────────────────────────────────────────────────────────────

wavefront-covers :
  ∀ {r c} (ix : RectIx (shape2 (suc r) (suc c))) →
  elemAt (wavefrontEnum r c) (coverDiag ix) (coverPos ix) ≡ ix
wavefront-covers {r} {c} (row , col) = cong₂ _,_ row-eq col-eq
  where
    dN  = toℕ row + toℕ col
    d   = coverDiag (row , col)
    k   = iMin dN c

    -- toℕ d = dN
    d-toℕ : toℕ d ≡ dN
    d-toℕ = toℕ-fromℕ< (coverDiag-bound row col)

    -- toℕ (coverPos (row,col)) = toℕ row ∸ k
    -- (cast preserves toℕ by toℕ-cast)
    pos-toℕ : toℕ (coverPos (row , col)) ≡ toℕ row ∸ k
    pos-toℕ =
      trans
        (toℕ-cast _ (fromℕ< (coverPos-bound row col)))
        (toℕ-fromℕ< (coverPos-bound row col))

    -- iMin (toℕ d) c + toℕ pos = toℕ row
    rowN-eq :
      iMin (toℕ d) c + toℕ (coverPos (row , col)) ≡ toℕ row
    rowN-eq =
      trans
        (cong₂ _+_ (cong (λ m → m ∸ c) d-toℕ) pos-toℕ)
        (m+[n∸m]≡n (iMin≤toℕrow row col))

    -- toℕ d ∸ (iMin (toℕ d) c + toℕ pos) = toℕ col
    colN-eq :
      toℕ d ∸ (iMin (toℕ d) c + toℕ (coverPos (row , col))) ≡ toℕ col
    colN-eq =
      trans
        (cong₂ _∸_ d-toℕ rowN-eq)
        (m+n∸m≡n (toℕ row) (toℕ col))

    row-eq :
      fromℕ< (wavefront-rowBound d (coverPos (row , col))) ≡ row
    row-eq = toℕ-injective (trans (toℕ-fromℕ< _) rowN-eq)

    col-eq :
      fromℕ< (wavefront-colBound d (coverPos (row , col))) ≡ col
    col-eq = toℕ-injective (trans (toℕ-fromℕ< _) colN-eq)

-- ─────────────────────────────────────────────────────────────────────────────
-- Injectivity of the wavefront enumerator
-- ─────────────────────────────────────────────────────────────────────────────

private
  -- r ⊓ dN ≤ dN  (second-argument bound for min, proved by structural induction)
  r⊓dN≤dN : ∀ (r dN : ℕ) → r ⊓ℕ dN ≤ dN
  r⊓dN≤dN zero    _       = z≤n
  r⊓dN≤dN (suc _) zero    = z≤n
  r⊓dN≤dN (suc r) (suc d) = s≤s (r⊓dN≤dN r d)

  suc-inj : ∀ {m n : ℕ} → suc m ≡ suc n → m ≡ n
  suc-inj refl = refl

  -- Left cancellation of addition (defined here to avoid depending on
  -- stdlib name which varies across versions).
  +-left-cancel : ∀ (k : ℕ) {m n : ℕ} → k + m ≡ k + n → m ≡ n
  +-left-cancel zero    h = h
  +-left-cancel (suc k) h = +-left-cancel k (suc-inj h)

-- rowN = iMin (toℕ d) c + toℕ pos  ≤  toℕ d
rowN≤dN :
  ∀ {r c} (d : Fin (diagCount r c)) (pos : Fin (cellCount (toℕ d) r c)) →
  iMin (toℕ d) c + toℕ pos ≤ toℕ d
rowN≤dN {r} {c} d pos =
  ≤-trans
    (lt-add-∸-≤ (iMax (toℕ d) r) (iMin (toℕ d) c) (toℕ pos) (toℕ<n pos))
    (r⊓dN≤dN r (toℕ d))

-- The row and column components of any wavefront element sum to the
-- diagonal index: toℕ row + toℕ col = toℕ d.
wavefront-diag-sum :
  ∀ {r c} (d : Fin (diagCount r c)) (pos : Fin (cellCount (toℕ d) r c)) →
  toℕ (proj₁ (elemAt (wavefrontEnum r c) d pos)) +
  toℕ (proj₂ (elemAt (wavefrontEnum r c) d pos)) ≡ toℕ d
wavefront-diag-sum {r} {c} d pos =
  trans
    (cong₂ _+_ (toℕ-fromℕ< (wavefront-rowBound {r} {c} d pos))
               (toℕ-fromℕ< (wavefront-colBound {r} {c} d pos)))
    (m+[n∸m]≡n (rowN≤dN {r} {c} d pos))

-- Two elements on different diagonals are unequal: equal elements imply
-- equal diagonals.
wavefront-injective-outer :
  ∀ {r c}
    (d1 d2 : Fin (diagCount r c))
    (p1 : Fin (cellCount (toℕ d1) r c))
    (p2 : Fin (cellCount (toℕ d2) r c)) →
  elemAt (wavefrontEnum r c) d1 p1 ≡ elemAt (wavefrontEnum r c) d2 p2 →
  d1 ≡ d2
wavefront-injective-outer {r} {c} d1 d2 p1 p2 heq =
  toℕ-injective
    (trans (sym (wavefront-diag-sum {r} {c} d1 p1))
      (trans
        (cong₂ _+_
          (cong toℕ (cong proj₁ heq))
          (cong toℕ (cong proj₂ heq)))
        (wavefront-diag-sum {r} {c} d2 p2)))

-- Two elements on the same diagonal at different positions are unequal:
-- equal elements on diagonal d imply equal positions.
wavefront-injective-inner :
  ∀ {r c}
    (d : Fin (diagCount r c))
    (p1 p2 : Fin (cellCount (toℕ d) r c)) →
  elemAt (wavefrontEnum r c) d p1 ≡ elemAt (wavefrontEnum r c) d p2 →
  p1 ≡ p2
wavefront-injective-inner {r} {c} d p1 p2 heq =
  toℕ-injective
    (+-left-cancel (iMin (toℕ d) c)
      (trans (sym (toℕ-fromℕ< (wavefront-rowBound {r} {c} d p1)))
        (trans (cong toℕ (cong proj₁ heq))
               (toℕ-fromℕ< (wavefront-rowBound {r} {c} d p2)))))
