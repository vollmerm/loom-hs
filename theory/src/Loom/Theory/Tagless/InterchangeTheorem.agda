{-# OPTIONS --safe #-}

-- Loop interchange expressed as a tagless interpreter swap.
--
-- The main theorem is:
--
--   copy-tagless-interchange :
--     lookupEnv (proj₁ (copy-prog (EnvAlg board) env)) arr ix ≡
--     lookupEnv (proj₁ (copy-prog (InterchangeAlg board) env)) arr ix
--
-- This says: running the SAME tagless program (copy-prog) under two different
-- algebra instances (EnvAlg = row-major, InterchangeAlg = col-major) produces
-- the same post-state.
--
-- There is no AST rewrite; loop interchange is expressed purely by substituting
-- one algebra for another.  The proof discharges via the existing deep-embedding
-- loop-interchange-preserves-state theorem, connected to the tagless layer by
-- a definitional (refl) bridge.

module Loom.Theory.Tagless.InterchangeTheorem where

open import Loom.Theory.Access using (read-only; write-only; rectAccess)
open import Loom.Theory.ExactCoverRect2 using (ExactCoverRect2Kernel)
open import Loom.Theory.Examples using (input; output; board; initial₂)
open import Loom.Theory.Index using (RectIx)
open import Loom.Theory.LoopInterchange using (loop-interchange-preserves-state)
open import Loom.Theory.Pointwise using (PointwiseKernel)
open import Loom.Theory.Prelude
open import Loom.Theory.RectExecution using (runRect2; runRect2Interchange)
open import Loom.Theory.Schedule using (rect)
open import Loom.Theory.Semantics using (Env; lookupEnv; updateEnv; readStore; runAt)
open import Loom.Theory.Shape using (rank2; shape2; Array; shape)
open import Loom.Theory.Syntax using (Program; program; read; write)
open import Loom.Theory.Tagless.InterchangeInterp using (InterchangeAlg)
open import Loom.Theory.Tagless.Program using (copy-prog; inc-prog)
open import Loom.Theory.Tagless.StateInterp using (EnvAlg)
open import Loom.Theory.WholeRect2 using (WholeRect2Kernel)

-- ─────────────────────────────────────────────────────────────────────────────
-- Deep copy program for the 8×8 board
-- ─────────────────────────────────────────────────────────────────────────────
--
-- This is the deep-embedding counterpart of the tagless copy-prog.  It is
-- identical in structure to Examples.tiny-rect-copy but for `board`
-- (shape2 8 8) and the `input`/`output` arrays.

board-rect-copy : Program rect board
board-rect-copy = program λ ix →
  read read-only (rectAccess input ix) λ value →
    write write-only (rectAccess output ix) value

-- ─────────────────────────────────────────────────────────────────────────────
-- Kernel infrastructure for the copy program
-- ─────────────────────────────────────────────────────────────────────────────

board-copy-kernel : PointwiseKernel rect board
board-copy-kernel = record
  { inputArr  = input
  ; outputArr = output
  ; distinct  = λ ()
  ; inputAt   = rectAccess input
  ; outputAt  = rectAccess output
  ; transform = λ value → value
  }

board-copy-whole-kernel : WholeRect2Kernel 8 8
board-copy-whole-kernel = record
  { base        = board-copy-kernel
  ; outputUnique = λ eq → eq
  }

board-copy-exact-kernel : ExactCoverRect2Kernel 8 8
board-copy-exact-kernel = record
  { wholeKernel   = board-copy-whole-kernel
  ; coverRow      = proj₁
  ; coverCol      = proj₂
  ; outputCovered = λ target → refl
  }

-- ─────────────────────────────────────────────────────────────────────────────
-- Bridge: tagless execution equals deep-embedding execution
-- ─────────────────────────────────────────────────────────────────────────────
--
-- Both hold definitionally (refl) because the tagless StateM interpreter
-- reduces to the same operations as evalKernel.

-- Running copy-body under EnvAlg at a given index equals runAt
copy-body-EnvAlg-eq :
  ∀ (env : Env rank2) (ix : RectIx board) →
  proj₁ (copy-prog (EnvAlg board) env) ≡ runRect2 env board-rect-copy
copy-body-EnvAlg-eq env ix = refl

-- Running copy-prog under InterchangeAlg equals runRect2Interchange
copy-body-InterchangeAlg-eq :
  ∀ (env : Env rank2) (ix : RectIx board) →
  proj₁ (copy-prog (InterchangeAlg board) env) ≡ runRect2Interchange env board-rect-copy
copy-body-InterchangeAlg-eq env ix = refl

-- ─────────────────────────────────────────────────────────────────────────────
-- Main theorem: loop interchange via algebra substitution
-- ─────────────────────────────────────────────────────────────────────────────
--
-- The same tagless copy-prog run under EnvAlg (row-major) and InterchangeAlg
-- (column-major) produces the same post-state.

copy-tagless-interchange :
  ∀ (env : Env rank2)
  (arr : Array rank2)
  (ix  : RectIx (shape arr)) →
  lookupEnv (proj₁ (copy-prog (EnvAlg board) env)) arr ix ≡
  lookupEnv (proj₁ (copy-prog (InterchangeAlg board) env)) arr ix
copy-tagless-interchange env arr ix =
  loop-interchange-preserves-state board-copy-exact-kernel env arr ix

-- ─────────────────────────────────────────────────────────────────────────────
-- Deep inc program for the 8×8 board
-- ─────────────────────────────────────────────────────────────────────────────

board-rect-inc : Program rect board
board-rect-inc = program λ ix →
  read read-only (rectAccess input ix) λ value →
    write write-only (rectAccess output ix) (suc value)

board-inc-kernel : PointwiseKernel rect board
board-inc-kernel = record
  { inputArr  = input
  ; outputArr = output
  ; distinct  = λ ()
  ; inputAt   = rectAccess input
  ; outputAt  = rectAccess output
  ; transform = suc
  }

board-inc-whole-kernel : WholeRect2Kernel 8 8
board-inc-whole-kernel = record
  { base        = board-inc-kernel
  ; outputUnique = λ eq → eq
  }

board-inc-exact-kernel : ExactCoverRect2Kernel 8 8
board-inc-exact-kernel = record
  { wholeKernel   = board-inc-whole-kernel
  ; coverRow      = proj₁
  ; coverCol      = proj₂
  ; outputCovered = λ target → refl
  }

inc-tagless-interchange :
  ∀ (env : Env rank2)
  (arr : Array rank2)
  (ix  : RectIx (shape arr)) →
  lookupEnv (proj₁ (inc-prog (EnvAlg board) env)) arr ix ≡
  lookupEnv (proj₁ (inc-prog (InterchangeAlg board) env)) arr ix
inc-tagless-interchange env arr ix =
  loop-interchange-preserves-state board-inc-exact-kernel env arr ix
