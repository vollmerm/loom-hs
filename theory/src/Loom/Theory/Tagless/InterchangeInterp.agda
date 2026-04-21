{-# OPTIONS --safe #-}

-- The interchange transformation expressed as a KernelAlg instance.
--
-- InterchangeAlg is identical to EnvAlg except for alg-for: instead of
-- enumerating indices in row-major order (outer = rows, inner = cols),
-- it enumerates in column-major order (outer = cols, inner = rows).
--
-- This is the key "interpreter as transformation" example:
--
--   Loop interchange in the Haskell library is implemented by writing a
--   Loop repr instance whose loopParFor2# swaps the two loop bounds.
--   There is no AST rewrite; the program just runs under a different
--   interpreter.
--
-- InterchangeAlg is the Agda counterpart of that idea.
--
-- The composition theorem (InterchangeTheorem) will prove that for any
-- well-formed tagless program p, running p under EnvAlg and under
-- InterchangeAlg produces the same post-state — not because we analysed
-- the program structure, but because the two algebras satisfy an AlgSim
-- relation and the program is algebra-oblivious.

module Loom.Theory.Tagless.InterchangeInterp where

open import Loom.Theory.Access using (CanRead; CanWrite)
open import Loom.Theory.Index using (Index; RectIx)
open import Loom.Theory.Prelude
open import Loom.Theory.Schedule using (rect)
open import Loom.Theory.Semantics using (Env; readStore; updateEnv)
open import Loom.Theory.Shape using (Rank; Shape; rank2; shape1; shape2; Array)
open import Loom.Theory.Tagless.Algebra using (KernelAlg)
open import Loom.Theory.Tagless.StateInterp using (StateM; stateReturn; stateBind)
open import Loom.Theory.Traversal using (foldFin)

-- ─────────────────────────────────────────────────────────────────────────────
-- Column-major domain fold
-- ─────────────────────────────────────────────────────────────────────────────

-- foldDomainInterchange is the column-major counterpart of foldDomain:
-- the outer loop runs over columns and the inner loop over rows.
-- For shape1 it degenerates to the same traversal as foldDomain.

foldDomainInterchange :
  ∀ {rank} →
  (dom : Shape rank) →
  (Index rect dom → Env rank → Env rank) →
  Env rank → Env rank
foldDomainInterchange (shape1 n) f env =
  foldFin (λ ix env′ → f ix env′) env
foldDomainInterchange (shape2 rows cols) f env =
  foldFin
    (λ col env′ →
      foldFin (λ row env″ → f (row , col) env″) env′)
    env

-- ─────────────────────────────────────────────────────────────────────────────
-- The interchange KernelAlg instance
-- ─────────────────────────────────────────────────────────────────────────────

-- InterchangeAlg is structurally identical to EnvAlg (same monad, same
-- alg-read, same alg-write) but uses foldDomainInterchange for alg-for.
--
-- This means: for a 2D rect domain (shape2 rows cols), EnvAlg iterates
--   for row in [0..rows): for col in [0..cols): body (row, col)
-- whereas InterchangeAlg iterates
--   for col in [0..cols): for row in [0..rows): body (row, col)

InterchangeAlg :
  ∀ {rank : Rank}
  (dom   : Shape rank) →
  KernelAlg rect dom (StateM rank)
InterchangeAlg dom = record
  { alg-return = stateReturn
  ; alg-bind   = stateBind
  ; alg-read   = λ {_} {arr} readable access env →
      env , readStore (env arr) access
  ; alg-write  = λ {_} {arr} writable access value env →
      updateEnv env arr access value , tt
  ; alg-for    = λ body env →
      foldDomainInterchange dom (λ ix env′ → proj₁ (body ix env′)) env , tt
  }

-- ─────────────────────────────────────────────────────────────────────────────
-- Relating EnvAlg and InterchangeAlg
-- ─────────────────────────────────────────────────────────────────────────────
--
-- The following three lemmas establish that the two algebras agree on every
-- operation except alg-for.  They will be assembled into a full AlgSim in
-- InterchangeTheorem.

-- ─────────────────────────────────────────────────────────────────────────────
-- Note on EnvAlg vs InterchangeAlg
-- ─────────────────────────────────────────────────────────────────────────────
--
-- alg-return, alg-bind, alg-read, and alg-write are definitionally equal
-- between EnvAlg and InterchangeAlg (both reduce to the same StateM operations).
-- Only alg-for differs: EnvAlg uses row-major traversal, InterchangeAlg uses
-- column-major traversal.
--
-- This definitional equality is exploited in InterchangeTheorem when building
-- the AlgSim proof: the non-for fields are filled by refl, and only sim-for
-- requires a non-trivial proof via LoopInterchange / foldFin-product.
