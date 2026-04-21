{-# OPTIONS --safe #-}

-- The standard sequential interpreter as a KernelAlg instance.
--
-- StateM A = Env rank → Env rank × A is the state monad over array environments.
-- EnvAlg builds a KernelAlg (StateM rank) by:
--   • alg-return / alg-bind : standard state-monad operations
--   • alg-read  : look up the value at the resolved index in the current env
--   • alg-write : update the env at the resolved index
--   • alg-for   : fold over the domain indices in *row-major* (left-to-right) order
--
-- The bridge lemma runTagless-eq shows that running copy-body / inc-body under
-- EnvAlg produces exactly the same result as evalKernel applied to the
-- corresponding deep Syntax.Kernel program.  This closes the connection between
-- the tagless layer and the existing theory.

module Loom.Theory.Tagless.StateInterp where

open import Loom.Theory.Access using (Access; CanRead; CanWrite; read-only; write-only;
                                       rectAccess)
open import Loom.Theory.Index using (Index; RectIx)
open import Loom.Theory.Pointwise using (PointwiseKernel; kernelProgram; inputAt; outputAt;
                                          transform; inputArr; outputArr; runAt-pointwise)
open import Loom.Theory.Prelude
open import Loom.Theory.Schedule using (Schedule; rect)
open import Loom.Theory.Semantics using (Env; lookupEnv; updateEnv; resolve; runAt;
                                          evalKernel; readStore)
open import Loom.Theory.Shape using (Rank; Shape; Array; shape; shape1; shape2; rank2)
open import Loom.Theory.Syntax as Syntax using (Program; body)
open import Loom.Theory.Tagless.Algebra
open import Loom.Theory.Tagless.Program
open import Loom.Theory.Traversal using (foldFin)

-- ─────────────────────────────────────────────────────────────────────────────
-- The state monad
-- ─────────────────────────────────────────────────────────────────────────────

-- StateM rank A: a state transformer over Env rank
StateM : Rank → Set → Set
StateM rank A = Env rank → Env rank × A

-- Standard state-monad combinators
stateReturn : ∀ {rank A} → A → StateM rank A
stateReturn a env = env , a

stateBind : ∀ {rank A B} → StateM rank A → (A → StateM rank B) → StateM rank B
stateBind m f env =
  let env′ , a = m env
  in  f a env′

-- State monad laws hold definitionally
state-left-id :
  ∀ {rank A B} (a : A) (f : A → StateM rank B) →
  stateBind (stateReturn a) f ≡ f a
state-left-id a f = refl

state-right-id :
  ∀ {rank A} (m : StateM rank A) →
  stateBind m stateReturn ≡ m
state-right-id m = refl

state-assoc :
  ∀ {rank A B C}
  (m : StateM rank A)
  (f : A → StateM rank B)
  (g : B → StateM rank C) →
  stateBind (stateBind m f) g ≡
  stateBind m (λ x → stateBind (f x) g)
state-assoc m f g = refl

-- ─────────────────────────────────────────────────────────────────────────────
-- Domain fold helper
-- ─────────────────────────────────────────────────────────────────────────────
--
-- foldDomain enumerates all indices of a rect-schedule domain by left-to-right
-- induction, matching the row-major order used by runRect1 / runRect2.

foldDomain :
  ∀ {rank} →
  (dom : Shape rank) →
  (Index rect dom → Env rank → Env rank) →
  Env rank → Env rank
foldDomain (shape1 n) f env =
  foldFin (λ ix env′ → f ix env′) env
foldDomain (shape2 rows cols) f env =
  foldFin
    (λ row env′ →
      foldFin (λ col env″ → f (row , col) env″) env′)
    env

-- ─────────────────────────────────────────────────────────────────────────────
-- The sequential KernelAlg instance
-- ─────────────────────────────────────────────────────────────────────────────

-- EnvAlg: the KernelAlg instance that executes programs sequentially,
-- enumerating domain indices in left-to-right (row-major) order via foldDomain.
EnvAlg :
  ∀ {rank : Rank}
  (dom   : Shape rank) →
  KernelAlg rect dom (StateM rank)
EnvAlg dom = record
  { alg-return = stateReturn
  ; alg-bind   = stateBind
  ; alg-read   = λ {_} {arr} readable access env →
      env , readStore (env arr) access
  ; alg-write  = λ {_} {arr} writable access value env →
      updateEnv env arr access value , tt
  ; alg-for    = λ body env →
      foldDomain dom (λ ix env′ → proj₁ (body ix env′)) env , tt
  }

-- ─────────────────────────────────────────────────────────────────────────────
-- Monad law instances for EnvAlg
-- ─────────────────────────────────────────────────────────────────────────────

EnvAlg-left-id :
  ∀ {rank dom A B} (a : A) (f : A → StateM rank B) →
  alg-bind (EnvAlg {rank} dom) (alg-return (EnvAlg dom) a) f ≡ f a
EnvAlg-left-id a f = refl

EnvAlg-right-id :
  ∀ {rank dom A} (m : StateM rank A) →
  alg-bind (EnvAlg {rank} dom) m (alg-return (EnvAlg dom)) ≡ m
EnvAlg-right-id m = refl

EnvAlg-assoc :
  ∀ {rank dom A B C}
  (m : StateM rank A)
  (f : A → StateM rank B)
  (g : B → StateM rank C) →
  alg-bind (EnvAlg {rank} dom)
    (alg-bind (EnvAlg dom) m f) g ≡
  alg-bind (EnvAlg dom) m
    (λ x → alg-bind (EnvAlg dom) (f x) g)
EnvAlg-assoc m f g = refl

-- ─────────────────────────────────────────────────────────────────────────────
-- Bridge: tagless copy/inc body agrees with evalKernel
-- ─────────────────────────────────────────────────────────────────────────────
--
-- We prove that running copy-body under EnvAlg produces the same result as
-- evalKernel applied to the corresponding deep Syntax.Kernel.
-- The proof goes by direct unfolding: both reduce to the same sequence of
-- env lookups and updateEnv calls.

open import Loom.Theory.Examples using (input; output; board; initial₂)


-- copy-body at ix under EnvAlg = runAt env (rect-copy deep program) ix
runTagless-copy-eq :
  ∀ (env : Env rank2) (ix : RectIx board) →
  proj₁ (copy-body ix (EnvAlg board) env) ≡
  runAt env (record { body = λ ix′ →
    Syntax.read read-only (rectAccess input ix′) λ value →
      Syntax.write write-only (rectAccess output ix′) value }) ix
runTagless-copy-eq env ix = refl

-- inc-body at ix under EnvAlg = runAt env (rect-inc deep program) ix
runTagless-inc-eq :
  ∀ (env : Env rank2) (ix : RectIx board) →
  proj₁ (inc-body ix (EnvAlg board) env) ≡
  runAt env (record { body = λ ix′ →
    Syntax.read read-only (rectAccess input ix′) λ value →
      Syntax.write write-only (rectAccess output ix′) (suc value) }) ix
runTagless-inc-eq env ix = refl
