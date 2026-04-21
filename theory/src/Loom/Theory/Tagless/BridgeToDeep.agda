{-# OPTIONS --safe #-}

-- Bridge between the tagless and deep-embedding interpretations.
--
-- The core claim is:
--   toTagless (program body) alg env = runAt env (program body) ix
--
-- for any index ix and any algebra alg that agrees with the deep-embedding
-- evaluator on read/write/bind/return.
--
-- For the specific case of EnvAlg, this holds definitionally (by refl).
-- This module makes the correspondence explicit by:
--
--  1. Defining toTagless : Program sched dom → Index sched dom → TaglessProg sched dom ⊤
--     which converts a single-step deep program body to tagless form.
--
--  2. Proving copy-body-bridge and inc-body-bridge: each tagless body at ix
--     under EnvAlg equals runAt env (deep program) ix (by refl).
--
--  3. Providing the whole-program bridges for copy-prog and inc-prog.

module Loom.Theory.Tagless.BridgeToDeep where

open import Loom.Theory.Access using (read-only; write-only; rectAccess; CanRead; CanWrite; Access)
open import Loom.Theory.Examples using (input; output; board; initial₂)
open import Loom.Theory.Index using (RectIx; Index)
open import Loom.Theory.Prelude
open import Loom.Theory.RectExecution using (runRect2)
open import Loom.Theory.Schedule using (rect; Schedule)
open import Loom.Theory.Semantics using (Env; lookupEnv; updateEnv; readStore; runAt)
open import Loom.Theory.Shape using (rank2; Rank; Shape)
open import Loom.Theory.Syntax using (Program; program; Kernel; unit; ⟦_⟧Ty)
open import Loom.Theory.Tagless.Algebra using
    ( KernelAlg; AlgSim; alg-return; alg-bind; alg-read; alg-write; alg-for
    ; sim-return; sim-bind; sim-read; sim-write )
open import Loom.Theory.Tagless.InterchangeTheorem using (board-rect-copy; board-rect-inc)
open import Loom.Theory.Tagless.Program using
    ( TaglessProg; bindProg; readProg; writeProg; forProg
    ; copy-body; inc-body; copy-prog; inc-prog )
open import Loom.Theory.Tagless.StateInterp using (StateM; EnvAlg)

-- ─────────────────────────────────────────────────────────────────────────────
-- Tagless evaluator for deep Kernel terms
-- ─────────────────────────────────────────────────────────────────────────────
--
-- toTaglessKernel interprets a Kernel sched unit term as a TaglessProg.
-- This shows any deep kernel is an instance of the tagless algebra.

toTaglessKernel :
  ∀ {rank sched dom ty} {repr : Set → Set} →
  KernelAlg {rank} sched dom repr →
  Kernel {rank} sched ty →
  repr (⟦ ty ⟧Ty)
toTaglessKernel alg (Loom.Theory.Syntax.pure val)    = alg-return alg val
toTaglessKernel alg (m Loom.Theory.Syntax.>>= f)     =
  alg-bind alg (toTaglessKernel alg m) (λ a → toTaglessKernel alg (f a))
toTaglessKernel alg (Loom.Theory.Syntax.read readable access next) =
  alg-bind alg (alg-read alg readable access) (λ val → toTaglessKernel alg (next val))
toTaglessKernel alg (Loom.Theory.Syntax.write writable access value) =
  alg-write alg writable access value

-- toTagless converts a deep Program body at a specific index to TaglessProg
toTagless :
  ∀ {rank sched dom} →
  Program {rank} sched dom →
  Index sched dom →
  TaglessProg sched dom ⊤
toTagless prog ix alg = toTaglessKernel alg (Loom.Theory.Syntax.Program.body prog ix)

-- ─────────────────────────────────────────────────────────────────────────────
-- Bridge correctness
-- ─────────────────────────────────────────────────────────────────────────────
--
-- Running a deep program body via toTagless + EnvAlg equals runAt.
-- This holds definitionally (by refl) because EnvAlg's alg-read/alg-write
-- reduce to exactly readStore/updateEnv, which is what evalKernel does.

toTagless-copy-bridge :
  ∀ (env : Env rank2) (ix : RectIx board) →
  proj₁ (toTagless board-rect-copy ix (EnvAlg board) env) ≡
  runAt env board-rect-copy ix
toTagless-copy-bridge env ix = refl

toTagless-inc-bridge :
  ∀ (env : Env rank2) (ix : RectIx board) →
  proj₁ (toTagless board-rect-inc ix (EnvAlg board) env) ≡
  runAt env board-rect-inc ix
toTagless-inc-bridge env ix = refl

-- ─────────────────────────────────────────────────────────────────────────────
-- Whole-program bridges
-- ─────────────────────────────────────────────────────────────────────────────
--
-- The tagless copy-prog and inc-prog are definitionally equal to the
-- deep runRect2 executions.

copy-prog-bridge :
  ∀ (env : Env rank2) →
  proj₁ (copy-prog (EnvAlg board) env) ≡ runRect2 env board-rect-copy
copy-prog-bridge env = refl

inc-prog-bridge :
  ∀ (env : Env rank2) →
  proj₁ (inc-prog (EnvAlg board) env) ≡ runRect2 env board-rect-inc
inc-prog-bridge env = refl

-- ─────────────────────────────────────────────────────────────────────────────
-- Preservation of tagless → deep (copy-body eq to toTagless)
-- ─────────────────────────────────────────────────────────────────────────────
--
-- The tagless smart-constructor-style copy-body is definitionally equal to
-- toTagless applied to the deep copy program body.  This confirms the two
-- representations describe identical computations.

copy-body-eq-toTagless :
  ∀ (ix : RectIx board) (alg : KernelAlg rect board (StateM rank2)) (env : Env rank2) →
  copy-body ix alg env ≡ toTagless board-rect-copy ix alg env
copy-body-eq-toTagless ix alg env = refl

-- ─────────────────────────────────────────────────────────────────────────────
-- The structural free theorem
-- ─────────────────────────────────────────────────────────────────────────────
--
-- toTaglessKernel-sim is the key result that the deep embedding was missing.
-- It says: for any two algebras alg₁, alg₂ that satisfy AlgSim R, interpreting
-- the SAME deep Kernel under both produces R-related results.
--
-- The proof goes by structural induction on the Kernel GADT:
--   • pure    : sim-return
--   • _>>=_   : sim-bind  + inductive hypothesis on both branches
--   • read    : sim-bind  + sim-read + inductive hypothesis on continuation
--   • write   : sim-write
--
-- This is the explicit substitute for the parametricity / free-theorem argument
-- that holds for tagless programs in System F but must be proven structurally
-- when programs are defined via the deep GADT.
--
-- Corollary: any deep Program body lifted via toTaglessKernel belongs to the
-- class of programs that can be freely translated between algebras.

toTaglessKernel-sim :
  ∀ {rank sched dom ty}
  {M₁ M₂ : Set → Set}
  {R : ∀ {A} → M₁ A → M₂ A → Set}
  {alg₁ : KernelAlg {rank} sched dom M₁}
  {alg₂ : KernelAlg {rank} sched dom M₂} →
  AlgSim R alg₁ alg₂ →
  (k : Kernel {rank} sched ty) →
  R (toTaglessKernel alg₁ k) (toTaglessKernel alg₂ k)
toTaglessKernel-sim {alg₁ = a₁} {alg₂ = a₂} sim k = go k
  where
    go : ∀ {ty′} (k′ : Kernel _ ty′) → _
    go (Loom.Theory.Syntax.pure val) =
      sim-return sim val
    go (m Loom.Theory.Syntax.>>= f) =
      sim-bind sim
        (toTaglessKernel a₁ m) (toTaglessKernel a₂ m)
        (go m)
        (λ a → toTaglessKernel a₁ (f a))
        (λ a → toTaglessKernel a₂ (f a))
        (λ a → go (f a))
    go (Loom.Theory.Syntax.read readable access next) =
      sim-bind sim
        (alg-read a₁ readable access) (alg-read a₂ readable access)
        (sim-read sim readable access)
        (λ val → toTaglessKernel a₁ (next val))
        (λ val → toTaglessKernel a₂ (next val))
        (λ val → go (next val))
    go (Loom.Theory.Syntax.write writable access value) =
      sim-write sim writable access value

-- toTagless-sim: the free theorem for full program bodies at any index.
-- Lifts toTaglessKernel-sim from single Kernel terms to indexed Program bodies.
toTagless-sim :
  ∀ {rank sched dom}
  {M₁ M₂ : Set → Set}
  {R : ∀ {A} → M₁ A → M₂ A → Set}
  {alg₁ : KernelAlg {rank} sched dom M₁}
  {alg₂ : KernelAlg {rank} sched dom M₂} →
  AlgSim R alg₁ alg₂ →
  (prog : Program {rank} sched dom) →
  (ix   : Index sched dom) →
  R (toTagless prog ix alg₁) (toTagless prog ix alg₂)
toTagless-sim sim prog ix = toTaglessKernel-sim sim (Loom.Theory.Syntax.Program.body prog ix)

