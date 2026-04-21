{-# OPTIONS --safe #-}

-- This module proves the confluent-reduction theorem: for any
-- commutative-associative reducer, foldReducer produces the same result
-- regardless of the traversal order in which the array elements are visited.
--
-- MOTIVATION: ReductionTheorems proves that foldReducer sumReducer equals
-- sumFin (a fixed-order sum) for the canonical traversal.  The paper's claim
-- is stronger: any valid traversal order gives the same fold.  This is the
-- reduction analog of schedule-equivalence, and it is what justifies foldFor1D
-- in the Loom implementation.
--
-- KEY DEFINITIONS
--
--   IsCommAssocReducer r :
--     step r (step r acc v1) v2 ≡ step r (step r acc v2) v1
--     (adjacent steps can be swapped; this generates all permutations)
--
--   PermOn n :
--     A bijection on Fin n (injective + surjective function Fin n → Fin n).
--     Represents an arbitrary traversal order of n elements.
--
-- KEY LEMMAS (proved by induction on n, no funext required)
--
--   foldReducerFrom-cong : pointwise equality of values implies equality of folds.
--
--   foldReducerFrom-head : a pending value can be pushed past the whole fold.
--     foldReducerFrom r (step r acc v) values
--       ≡ step r (foldReducerFrom r acc values) v
--
--   foldReducerFrom-head-k : pick any position k, move it to the end.
--     foldReducerFrom r acc values
--       ≡ step r (foldReducerFrom r acc (values ∘ punchIn k)) (values k)
--
-- HEADLINE THEOREM
--
--   foldReducer-perm :
--     IsCommAssocReducer r → PermOn n → (values : Fin n → ℕ) →
--     foldReducer r (values ∘ fun p) ≡ foldReducer r values
--
-- CONCRETE INSTANCES
--
--   sumReducer-is-comm-assoc : IsCommAssocReducer sumReducer
--   foldReducer-sum-perm     : sumReducer satisfies the general theorem

module Loom.Theory.ConfluenceReduction where

open import Data.Fin.Base using (punchIn; punchOut)
open import Data.Fin.Properties
  using ( punchIn-injective
        ; punchInᵢ≢i
        ; punchIn-punchOut
        ; punchOut-punchIn
        ; punchOut-cong
        ; punchOut-injective
        )
open import Function.Base using (_∘_)
open import Data.Nat.Properties as ℕₚ using (+-comm; +-assoc)
open import Data.Product.Base using (Σ; _,_)
open import Loom.Theory.Prelude
open import Loom.Theory.Reduction

-- ────────────────────────────────────────────────────────────────────────────
-- IsCommAssocReducer predicate
-- ────────────────────────────────────────────────────────────────────────────

-- A reducer is commutative-associative when any two consecutive fold steps
-- can be swapped.  This is the minimal algebraic condition needed to prove
-- that foldReducer is invariant under arbitrary permutations of its input.
record IsCommAssocReducer (r : Reducer) : Set where
  field
    comm-step : ∀ (acc : Carrier r) (v1 v2 : ℕ) →
      step r (step r acc v1) v2 ≡ step r (step r acc v2) v1

open IsCommAssocReducer public

-- ────────────────────────────────────────────────────────────────────────────
-- Key lemma 1: foldReducerFrom-cong
-- ────────────────────────────────────────────────────────────────────────────

-- Pointwise equality of values implies equality of folds.
-- (Avoids the need for funext.)
foldReducerFrom-cong :
  ∀ {n} (r : Reducer) (acc : Carrier r) {f g : Fin n → ℕ} →
  (∀ i → f i ≡ g i) →
  foldReducerFrom r acc f ≡ foldReducerFrom r acc g
foldReducerFrom-cong {zero}  r acc h = refl
foldReducerFrom-cong {suc n} r acc {f} {g} h =
  trans
    (cong (λ v → foldReducerFrom r (step r acc v) (f ∘ fsuc)) (h fzero))
    (foldReducerFrom-cong r (step r acc (g fzero)) (λ i → h (fsuc i)))

-- ────────────────────────────────────────────────────────────────────────────
-- Key lemma 2: foldReducerFrom-head
-- ────────────────────────────────────────────────────────────────────────────

-- A single pending value can be "pushed through" the whole fold to the end.
--
--   foldReducerFrom r (step r acc v) values
--     ≡ step r (foldReducerFrom r acc values) v
--
-- Proof by induction on n, using comm-step to move v past the next value at
-- each step.
foldReducerFrom-head :
  ∀ {n} (r : Reducer) → IsCommAssocReducer r →
  (acc : Carrier r) (v : ℕ) (values : Fin n → ℕ) →
  foldReducerFrom r (step r acc v) values ≡
    step r (foldReducerFrom r acc values) v
foldReducerFrom-head {zero}  r _  acc v values = refl
foldReducerFrom-head {suc n} r ca acc v values =
  trans
    -- Swap v past values fzero using comm-step.
    (cong (λ a → foldReducerFrom r a (values ∘ fsuc))
          (comm-step ca acc v (values fzero)))
    -- Then push v through the remaining tail by induction.
    (foldReducerFrom-head r ca (step r acc (values fzero)) v (values ∘ fsuc))

-- ────────────────────────────────────────────────────────────────────────────
-- Key lemma 3: foldReducerFrom-head-k
-- ────────────────────────────────────────────────────────────────────────────

-- Pick any element at position k and move it to the end of the fold.
--
--   foldReducerFrom r acc values
--     ≡ step r (foldReducerFrom r acc (values ∘ punchIn k)) (values k)
--
-- Proof:
--   k = fzero: immediate from foldReducerFrom-head.
--   k = fsuc k': unfold the first step of the fold (values fzero is consumed),
--     apply the IH to the tail, then observe that
--     values ∘ punchIn (fsuc k') and values ∘ fsuc ∘ punchIn k' are
--     definitionally equal (punchIn (fsuc k') ∘ fsuc = fsuc ∘ punchIn k').
foldReducerFrom-head-k :
  ∀ {n} (r : Reducer) → IsCommAssocReducer r →
  (acc : Carrier r) (k : Fin (suc n)) (values : Fin (suc n) → ℕ) →
  foldReducerFrom r acc values ≡
    step r (foldReducerFrom r acc (values ∘ punchIn k)) (values k)
foldReducerFrom-head-k r ca acc fzero values =
  -- punchIn fzero = fsuc definitionally, so this is exactly foldReducerFrom-head.
  foldReducerFrom-head r ca acc (values fzero) (values ∘ fsuc)
foldReducerFrom-head-k {suc n} r ca acc (fsuc k) values =
  -- foldReducerFrom r acc values
  --   = foldReducerFrom r (step r acc (values fzero)) (values ∘ fsuc)  [by def]
  --   = step r (foldReducerFrom r (step r acc (values fzero))
  --                               (values ∘ fsuc ∘ punchIn k))
  --            (values (fsuc k))                                         [IH]
  -- And foldReducerFrom r acc (values ∘ punchIn (fsuc k))
  --   = foldReducerFrom r (step r acc (values fzero))
  --                       (values ∘ fsuc ∘ punchIn k)           [by def, since
  --     punchIn (fsuc k) fzero = fzero and punchIn (fsuc k) ∘ fsuc = fsuc ∘ punchIn k]
  foldReducerFrom-head-k r ca (step r acc (values fzero)) k (values ∘ fsuc)

-- ────────────────────────────────────────────────────────────────────────────
-- Bijection type and restriction
-- ────────────────────────────────────────────────────────────────────────────

-- A bijection on Fin n: an injective surjective function.
record PermOn (n : ℕ) : Set where
  field
    fun        : Fin n → Fin n
    injective  : ∀ {i j} → fun i ≡ fun j → i ≡ j
    surjective : (k : Fin n) → Σ (Fin n) (λ i → fun i ≡ k)

open PermOn public

-- No-escape: punchIn k₀ never hits k₀.
punchIn-ne-k₀ :
  ∀ {n} (k₀ : Fin (suc n)) (i : Fin n) → punchIn k₀ i ≢ k₀
punchIn-ne-k₀ k₀ i = punchInᵢ≢i k₀ i

-- Restrict a bijection p : PermOn (suc n) to PermOn n by "removing" the
-- preimage of fzero.  Given k₀ with p.fun k₀ = fzero, define
--   fun' i = punchOut (fzero ≢ p.fun (punchIn k₀ i))
-- which maps Fin n → Fin n by first embedding into Fin(suc n) \ {k₀}
-- (via punchIn k₀), applying p.fun (landing in Fin(suc n) \ {fzero}),
-- then collapsing back to Fin n (via punchOut at fzero).
restrictPerm :
  ∀ {n} →
  (p   : PermOn (suc n)) →
  (k₀  : Fin (suc n)) →
  fun p k₀ ≡ fzero →
  PermOn n
restrictPerm {n} p k₀ pk₀≡0 = record
  { fun        = fun'
  ; injective  = fun'-inj
  ; surjective = fun'-sur
  }
  where
    -- p.fun avoids fzero on the image of punchIn k₀.
    ne : (i : Fin n) → fzero ≢ fun p (punchIn k₀ i)
    ne i h = punchInᵢ≢i k₀ i (sym (injective p (trans pk₀≡0 h)))

    fun' : Fin n → Fin n
    fun' i = punchOut (ne i)

    fun'-inj : ∀ {i j} → fun' i ≡ fun' j → i ≡ j
    fun'-inj {i} {j} h =
      punchIn-injective k₀ i j
        (injective p
          (trans
            (sym (punchIn-punchOut (ne i)))
            (trans (cong (punchIn fzero) h)
                   (punchIn-punchOut (ne j)))))

    fun'-sur : (k : Fin n) → Σ (Fin n) (λ i → fun' i ≡ k)
    fun'-sur k =
      -- Surjectivity of p at punchIn fzero k = fsuc k.
      let (m , pm≡sk) = surjective p (punchIn fzero k)
          -- m ≢ k₀: if m = k₀, then p.fun k₀ = fzero ≠ fsuc k.
          k₀≢m : k₀ ≢ m
          k₀≢m h = punchInᵢ≢i fzero k
                      (trans (sym pm≡sk) (trans (cong (fun p) (sym h)) pk₀≡0))
          -- punchIn k₀ (punchOut k₀≢m) = m
          i      = punchOut k₀≢m
          pi≡m   = punchIn-punchOut k₀≢m
          -- p.fun (punchIn k₀ i) = fsuc k
          fpi≡sk : fun p (punchIn k₀ i) ≡ punchIn fzero k
          fpi≡sk = trans (cong (fun p) pi≡m) pm≡sk
      in  i
        , trans
            (punchOut-cong fzero {i≢k = punchInᵢ≢i fzero k ∘ sym} fpi≡sk)
            (punchOut-punchIn fzero)

-- ────────────────────────────────────────────────────────────────────────────
-- HEADLINE THEOREM: foldReducerFrom-perm-inv
-- ────────────────────────────────────────────────────────────────────────────

-- For a commutative-associative reducer, foldReducerFrom is invariant under
-- arbitrary bijections of its index domain.
--
-- The internal lemma works at the foldReducerFrom level (accumulator is
-- explicit) so the induction goes through cleanly.  foldReducer-perm wraps
-- it by applying done r.
--
-- Proof by induction on n:
--   n = 0: trivial (empty fold).
--   n = suc n':
--     1. Pull out the element at position k₀ (the preimage of fzero under p)
--        using foldReducerFrom-head-k on the LHS.
--     2. Rewrite the inner fold using the restriction p' = restrictPerm p k₀
--        via foldReducerFrom-cong.
--     3. Apply the IH on n' to equate the inner fold with the canonical order.
--     4. Rewrite p.fun k₀ = fzero.
--     5. Sym of foldReducerFrom-head-k at position fzero on the RHS.
foldReducerFrom-perm-inv :
  ∀ {n} (r : Reducer) → IsCommAssocReducer r →
  (acc : Carrier r) →
  (p : PermOn n) →
  (values : Fin n → ℕ) →
  foldReducerFrom r acc (values ∘ fun p) ≡ foldReducerFrom r acc values
foldReducerFrom-perm-inv {zero}  r ca acc p values = refl
foldReducerFrom-perm-inv {suc n} r ca acc p values =
  let (k₀ , pk₀≡0) = surjective p fzero
      p'            = restrictPerm p k₀ pk₀≡0
      -- ne i : fzero ≢ fun p (punchIn k₀ i)  (local copy for reindex step)
      ne : (i : Fin n) → fzero ≢ fun p (punchIn k₀ i)
      ne i h = punchInᵢ≢i k₀ i (sym (injective p (trans pk₀≡0 h)))
      -- fun p (punchIn k₀ i) = punchIn fzero (fun p' i)
      -- Proof: sym (punchIn-punchOut (ne i)), since punchIn fzero (punchOut (ne i)) = fun p (punchIn k₀ i).
      reindex : (i : Fin n) → values (fun p (punchIn k₀ i)) ≡ (values ∘ fsuc) (fun p' i)
      reindex i = cong values (sym (punchIn-punchOut (ne i)))
  in
  trans
    -- Step 1: pull k₀ out of the LHS fold
    (foldReducerFrom-head-k r ca acc k₀ (values ∘ fun p))
  (trans
    -- Step 2: rewrite inner fold so it looks like (values ∘ fsuc) ∘ fun p'
    (cong (λ c → step r c (values (fun p k₀)))
          (foldReducerFrom-cong r acc reindex))
  (trans
    -- Step 3: apply IH at size n
    (cong (λ s → step r s (values (fun p k₀)))
          (foldReducerFrom-perm-inv r ca acc p' (values ∘ fsuc)))
  (trans
    -- Step 4: rewrite values (fun p k₀) to values fzero
    (cong (step r (foldReducerFrom r acc (values ∘ fsuc))) (cong values pk₀≡0))
    -- Step 5: sym of pulling fzero out on the RHS
    -- foldReducerFrom-head-k at fzero:
    --   foldReducerFrom r acc values
    --   ≡ step r (foldReducerFrom r acc (values ∘ punchIn fzero)) (values fzero)
    --   = step r (foldReducerFrom r acc (values ∘ fsuc)) (values fzero)  [punchIn fzero = fsuc]
    (sym (foldReducerFrom-head-k r ca acc fzero values)))))

-- HEADLINE THEOREM: foldReducer-perm.
--
-- For a commutative-associative reducer, foldReducer is invariant under
-- bijections of the index domain.
foldReducer-perm :
  ∀ {n} (r : Reducer) → IsCommAssocReducer r →
  (p : PermOn n) →
  (values : Fin n → ℕ) →
  foldReducer r (values ∘ fun p) ≡ foldReducer r values
foldReducer-perm r ca p values =
  cong (done r) (foldReducerFrom-perm-inv r ca (init r) p values)

-- ────────────────────────────────────────────────────────────────────────────
-- Concrete instance: sumReducer
-- ────────────────────────────────────────────────────────────────────────────

-- sumReducer is commutative-associative.
-- step acc v1 then v2 = acc + v1 + v2 = acc + v2 + v1 = step acc v2 then v1.
sumReducer-is-comm-assoc : IsCommAssocReducer sumReducer
sumReducer-is-comm-assoc = record
  { comm-step = λ acc v1 v2 →
      trans
        (+-assoc acc v1 v2)
        (trans
          (cong (acc +_) (+-comm v1 v2))
          (sym (+-assoc acc v2 v1)))
  }

-- COROLLARY: foldReducer sumReducer is traversal-order-independent.
foldReducer-sum-perm :
  ∀ {n} (p : PermOn n) (values : Fin n → ℕ) →
  foldReducer sumReducer (values ∘ fun p) ≡ foldReducer sumReducer values
foldReducer-sum-perm = foldReducer-perm sumReducer sumReducer-is-comm-assoc
