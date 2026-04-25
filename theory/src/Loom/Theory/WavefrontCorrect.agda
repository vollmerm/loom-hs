{-# OPTIONS --safe #-}

-- Correctness of the wavefront (anti-diagonal) schedule for 2D pointwise
-- kernels.
--
-- KEY THEOREM  (wavefront-correct)
--
--   For any WavefrontKernel on an (suc r × suc c) grid, running all
--   anti-diagonal phases via the wavefront enumerator produces the correct
--   value at every output cell:
--
--     lookupEnv (runEnumerator env (kernelProgram (base kernel))
--                               (wavefrontEnum r c))
--               (outputArr (base kernel))
--               (resolve (outputAt (base kernel)
--                          (elemAt (wavefrontEnum r c) d pos)))
--     ≡ transform (base kernel)
--         (lookupEnv env (inputArr (base kernel))
--           (resolve (inputAt (base kernel)
--                      (elemAt (wavefrontEnum r c) d pos))))
--
--   Proof strategy:
--     1. For each single anti-diagonal, delegate to WholeLinear.runWhole-pointwise.
--     2. An inductive sweep (runWF-outer-ind) over all diagonals shows that
--        earlier diagonals do not overwrite the target cell of diagonal d,
--        and input values are preserved throughout.

module Loom.Theory.WavefrontCorrect where

open import Loom.Theory.Access
open import Loom.Theory.Index
open import Loom.Theory.Pointwise
open import Loom.Theory.Prelude
open import Loom.Theory.Schedule
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.Traversal
  using (foldFin; runEnumerator; linearTraversal; elemAt)
open import Loom.Theory.WavefrontTraversal
open import Loom.Theory.WholeLinear
  using (WholeKernel; runWhole-pointwise; runWhole-input-preserved)

-- ─────────────────────────────────────────────────────────────────────────────
-- WavefrontKernel: a pointwise kernel on a 2D grid together with a proof
-- that its output-address function is injective.
-- ─────────────────────────────────────────────────────────────────────────────

record WavefrontKernel (r c : ℕ) : Set where
  field
    base      : PointwiseKernel rect (shape2 (suc r) (suc c))
    -- Distinct grid cells map to distinct output addresses.
    outputInj : ∀ (ix iy : RectIx (shape2 (suc r) (suc c))) →
                  resolve (outputAt base ix) ≡ resolve (outputAt base iy) → ix ≡ iy

open WavefrontKernel public

-- ─────────────────────────────────────────────────────────────────────────────
-- Private proof machinery
-- ─────────────────────────────────────────────────────────────────────────────

private

  -- Fin helpers.
  fsuc-inj : ∀ {n} {i j : Fin n} → fsuc i ≡ fsuc j → i ≡ j
  fsuc-inj refl = refl

  fsuc≢fzero : ∀ {n} {i : Fin n} → fsuc i ≢ fzero
  fsuc≢fzero ()

  -- If every step of a foldFin preserves lookupEnv env arr j, so does the
  -- full fold.
  foldFin-preserve :
    ∀ {n : ℕ} →
    (body : Fin n → Env rank2 → Env rank2) →
    (env  : Env rank2) →
    (arr  : Array rank2) →
    (j    : RectIx (shape arr)) →
    (∀ k env′ → lookupEnv (body k env′) arr j ≡ lookupEnv env′ arr j) →
    lookupEnv (foldFin body env) arr j ≡ lookupEnv env arr j
  foldFin-preserve {zero}  _    _   _   _ _    = refl
  foldFin-preserve {suc m} body env arr j pres =
    trans
      (foldFin-preserve (λ k env′ → body (fsuc k) env′) (body fzero env) arr j
         (λ k env′ → pres (fsuc k) env′))
      (pres fzero env)

  -- Single-diagonal execution: apply the pointwise kernel to every cell on
  -- anti-diagonal d.
  runDiag :
    ∀ {r c} →
    Env rank2 →
    PointwiseKernel rect (shape2 (suc r) (suc c)) →
    Fin (diagCount r c) →
    Env rank2
  runDiag {r} {c} env base d =
    foldFin
      (λ pos env′ → runAt env′ (kernelProgram base) (elemAt (wavefrontEnum r c) d pos))
      env

  -- Build a WholeKernel for a single anti-diagonal.
  -- outputUnique uses wavefront-injective-inner + outputInj.
  diagKernel :
    ∀ {r c} →
    (base      : PointwiseKernel rect (shape2 (suc r) (suc c))) →
    (outputInj : ∀ ix iy → resolve (outputAt base ix) ≡ resolve (outputAt base iy) → ix ≡ iy) →
    (d         : Fin (diagCount r c)) →
    WholeKernel {rank2} {rect} {shape2 (suc r) (suc c)} (cellCount (toℕ d) r c)
  diagKernel {r} {c} base outputInj d = record
    { base         = base
    ; steps        = linearTraversal (λ pos → elemAt (wavefrontEnum r c) d pos)
    ; outputUnique = λ {i} {j} heq →
        wavefront-injective-inner d i j (outputInj _ _ heq)
    }

  -- runDiag is definitionally equal to runWhole of diagKernel.
  runDiag≡runWhole :
    ∀ {r c}
    (base      : PointwiseKernel rect (shape2 (suc r) (suc c))) →
    (outputInj : ∀ ix iy → resolve (outputAt base ix) ≡ resolve (outputAt base iy) → ix ≡ iy) →
    (env : Env rank2) (d : Fin (diagCount r c)) →
    runDiag env base d ≡
      Loom.Theory.WholeLinear.runWhole env (diagKernel base outputInj d)
  runDiag≡runWhole _ _ _ _ = refl

  -- Pointwise correctness for a single diagonal.
  runDiag-pointwise :
    ∀ {r c}
    (base      : PointwiseKernel rect (shape2 (suc r) (suc c)))
    (outputInj : ∀ ix iy → resolve (outputAt base ix) ≡ resolve (outputAt base iy) → ix ≡ iy)
    (env : Env rank2)
    (d   : Fin (diagCount r c))
    (pos : Fin (cellCount (toℕ d) r c)) →
    lookupEnv (runDiag env base d) (outputArr base)
      (resolve (outputAt base (elemAt (wavefrontEnum r c) d pos))) ≡
    transform base
      (lookupEnv env (inputArr base)
        (resolve (inputAt base (elemAt (wavefrontEnum r c) d pos))))
  runDiag-pointwise base outputInj env d pos =
    runWhole-pointwise (diagKernel base outputInj d) env pos

  -- Running one diagonal preserves the input array everywhere.
  runDiag-input-pres :
    ∀ {r c}
    (base      : PointwiseKernel rect (shape2 (suc r) (suc c)))
    (outputInj : ∀ ix iy → resolve (outputAt base ix) ≡ resolve (outputAt base iy) → ix ≡ iy)
    (env : Env rank2)
    (d   : Fin (diagCount r c))
    (j   : RectIx (shape (inputArr base))) →
    lookupEnv (runDiag env base d) (inputArr base) j ≡ lookupEnv env (inputArr base) j
  runDiag-input-pres base outputInj env d j =
    runWhole-input-preserved (diagKernel base outputInj d) env j

  -- Running diagonal d1 does not affect the output cell belonging to
  -- diagonal d2, provided d1 ≢ d2.
  runDiag-noHit :
    ∀ {r c}
    (base      : PointwiseKernel rect (shape2 (suc r) (suc c)))
    (outputInj : ∀ ix iy → resolve (outputAt base ix) ≡ resolve (outputAt base iy) → ix ≡ iy)
    (env  : Env rank2)
    (d1 d2 : Fin (diagCount r c))
    (pos  : Fin (cellCount (toℕ d2) r c)) →
    d1 ≢ d2 →
    lookupEnv (runDiag env base d1) (outputArr base)
      (resolve (outputAt base (elemAt (wavefrontEnum r c) d2 pos))) ≡
    lookupEnv env (outputArr base)
      (resolve (outputAt base (elemAt (wavefrontEnum r c) d2 pos)))
  runDiag-noHit {r} {c} base outputInj env d1 d2 pos d1≢d2 =
    foldFin-preserve
      (λ pos′ env′ → runAt env′ (kernelProgram base) (elemAt (wavefrontEnum r c) d1 pos′))
      env
      (outputArr base)
      (resolve (outputAt base (elemAt (wavefrontEnum r c) d2 pos)))
      (λ pos′ env′ →
        updateEnv-other-index
          env′
          (outputArr base)
          (outputAt base (elemAt (wavefrontEnum r c) d1 pos′))
          (transform base
            (lookupEnv env′ (inputArr base)
              (resolve (inputAt base (elemAt (wavefrontEnum r c) d1 pos′)))))
          (resolve (outputAt base (elemAt (wavefrontEnum r c) d2 pos)))
          (λ eq → d1≢d2
            (wavefront-injective-outer d1 d2 pos′ pos
              (outputInj _ _ eq))))

  -- The main inductive engine.
  -- Induction on n (diagonals yet to process), with injective selector
  -- f : Fin n → Fin (diagCount r c).  The result at any cell (d, pos) equals
  -- the canonical pointwise output computed from env0.
  runWF-outer-ind :
    ∀ {r c}
    (base      : PointwiseKernel rect (shape2 (suc r) (suc c)))
    (outputInj : ∀ ix iy → resolve (outputAt base ix) ≡ resolve (outputAt base iy) → ix ≡ iy)
    (n         : ℕ)
    (f         : Fin n → Fin (diagCount r c))
    (f-inj     : ∀ i j → f i ≡ f j → i ≡ j)
    (env0 env  : Env rank2)
    (input-pres : ∀ j → lookupEnv env (inputArr base) j ≡
                         lookupEnv env0 (inputArr base) j)
    (d   : Fin n)
    (pos : Fin (cellCount (toℕ (f d)) r c)) →
    lookupEnv
      (foldFin (λ k env′ → runDiag env′ base (f k)) env)
      (outputArr base)
      (resolve (outputAt base (elemAt (wavefrontEnum r c) (f d) pos))) ≡
    transform base
      (lookupEnv env0 (inputArr base)
        (resolve (inputAt base (elemAt (wavefrontEnum r c) (f d) pos))))

  runWF-outer-ind _ _ zero _ _ _ _ _ () _

  -- Base case for the selected diagonal: fzero.  The suffix of diagonals
  -- (fsuc k) does not touch the target cell, so we reduce to runDiag-pointwise.
  runWF-outer-ind {r} {c} base outputInj (suc m) f f-inj env0 env input-pres fzero pos =
    trans
      (foldFin-preserve
        (λ k env′ → runDiag env′ base (f (fsuc k)))
        (runDiag env base (f fzero))
        (outputArr base)
        (resolve (outputAt base (elemAt (wavefrontEnum r c) (f fzero) pos)))
        (λ k env′ →
          runDiag-noHit base outputInj env′ (f (fsuc k)) (f fzero) pos
            (λ eq → fsuc≢fzero (f-inj (fsuc k) fzero eq))))
      (trans
        (runDiag-pointwise base outputInj env (f fzero) pos)
        (cong (transform base)
          (input-pres
            (resolve (inputAt base (elemAt (wavefrontEnum r c) (f fzero) pos))))))

  -- Inductive step: advance past diagonal fzero and recurse on the tail.
  runWF-outer-ind {r} {c} base outputInj (suc m) f f-inj env0 env input-pres (fsuc d′) pos =
    runWF-outer-ind base outputInj m
      (λ k → f (fsuc k))
      (λ i j eq → fsuc-inj (f-inj (fsuc i) (fsuc j) eq))
      env0
      (runDiag env base (f fzero))
      (λ j →
        trans
          (runDiag-input-pres base outputInj env (f fzero) j)
          (input-pres j))
      d′
      pos

-- ─────────────────────────────────────────────────────────────────────────────
-- Main theorems
-- ─────────────────────────────────────────────────────────────────────────────

-- Each output cell produced by the wavefront traversal equals the canonical
-- pointwise result computed from the initial environment.
wavefront-correct :
  ∀ {r c}
  (kernel : WavefrontKernel r c)
  (env    : Env rank2)
  (d      : Fin (diagCount r c))
  (pos    : Fin (cellCount (toℕ d) r c)) →
  lookupEnv
    (runEnumerator env (kernelProgram (base kernel)) (wavefrontEnum r c))
    (outputArr (base kernel))
    (resolve (outputAt (base kernel) (elemAt (wavefrontEnum r c) d pos))) ≡
  transform (base kernel)
    (lookupEnv env (inputArr (base kernel))
      (resolve (inputAt (base kernel) (elemAt (wavefrontEnum r c) d pos))))
wavefront-correct {r} {c} kernel env d pos =
  runWF-outer-ind
    (base kernel) (outputInj kernel)
    (diagCount r c) (λ k → k) (λ i j eq → eq)
    env env (λ j → refl)
    d pos

-- The wavefront traversal does not modify arrays other than the output array.
wavefront-unrelated :
  ∀ {r c}
  (kernel : WavefrontKernel r c)
  (env    : Env rank2)
  (arr    : Array rank2) →
  outputArr (base kernel) ≢ arr →
  (j : RectIx (shape arr)) →
  lookupEnv
    (runEnumerator env (kernelProgram (base kernel)) (wavefrontEnum r c))
    arr j ≡
  lookupEnv env arr j
wavefront-unrelated {r} {c} kernel env arr arr≢out j =
  foldFin-preserve
    (λ d env′ →
      foldFin
        (λ pos env″ →
          runAt env″ (kernelProgram (base kernel)) (elemAt (wavefrontEnum r c) d pos))
        env′)
    env arr j
    (λ d env′ →
      foldFin-preserve
        (λ pos env″ →
          runAt env″ (kernelProgram (base kernel)) (elemAt (wavefrontEnum r c) d pos))
        env′ arr j
        (λ pos env″ →
          runAt-unrelated (base kernel) env″
            (elemAt (wavefrontEnum r c) d pos) arr arr≢out j))

-- The input array is preserved by the wavefront traversal.
wavefront-input-preserved :
  ∀ {r c}
  (kernel : WavefrontKernel r c)
  (env    : Env rank2)
  (j      : RectIx (shape (inputArr (base kernel)))) →
  lookupEnv
    (runEnumerator env (kernelProgram (base kernel)) (wavefrontEnum r c))
    (inputArr (base kernel)) j ≡
  lookupEnv env (inputArr (base kernel)) j
wavefront-input-preserved kernel env j =
  wavefront-unrelated kernel env (inputArr (base kernel))
    (λ eq → distinct (base kernel) (sym eq)) j
