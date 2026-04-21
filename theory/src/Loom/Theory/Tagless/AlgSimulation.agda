{-# OPTIONS --safe #-}

-- Composition lemmas for AlgSim.
--
-- AlgSim R alg₁ alg₂ is stated at the level of individual operations.
-- The lemmas here show that if alg₁ and alg₂ are related by AlgSim, then
-- every smart constructor from Tagless.Program maps alg₁-results to
-- alg₂-results.
--
-- This is the explicit substitute for the free-theorem argument that would
-- hold automatically via parametricity in System F.  In Agda, we prove it
-- operation-by-operation and then combine.

module Loom.Theory.Tagless.AlgSimulation where

open import Loom.Theory.Prelude
open import Loom.Theory.Access using (read-only; write-only; rectAccess)
open import Loom.Theory.Index using (Index)
open import Loom.Theory.Schedule using (rect)
open import Loom.Theory.Shape using (Rank; Shape; rank2)
open import Loom.Theory.Tagless.Algebra using (KernelAlg; AlgSim;
    alg-return; alg-bind; alg-read; alg-write; alg-for; alg-seq;
    sim-return; sim-bind; sim-read; sim-write; sim-for)
open import Loom.Theory.Tagless.Program using
    ( TaglessProg
    ; returnProg; bindProg; readProg; writeProg; forProg
    ; copy-body; inc-body; copy-prog; inc-prog)

-- ─────────────────────────────────────────────────────────────────────────────
-- Derived simulation lemmas
-- ─────────────────────────────────────────────────────────────────────────────

-- sim-seq: sequential composition preserves the simulation relation.
sim-seq :
  ∀ {rank} {sched : _} {dom : Shape rank}
  {M₁ M₂ : Set → Set}
  {R : ∀ {A} → M₁ A → M₂ A → Set}
  {alg₁ : KernelAlg sched dom M₁}
  {alg₂ : KernelAlg sched dom M₂} →
  AlgSim R alg₁ alg₂ →
  ∀ {A B} →
  (m₁ : M₁ A) (m₂ : M₂ A) →
  R m₁ m₂ →
  (n₁ : M₁ B) (n₂ : M₂ B) →
  R n₁ n₂ →
  R (alg-seq alg₁ m₁ n₁) (alg-seq alg₂ m₂ n₂)
sim-seq sim m₁ m₂ Rm n₁ n₂ Rn =
  sim-bind sim m₁ m₂ Rm (λ _ → n₁) (λ _ → n₂) (λ _ → Rn)

-- ─────────────────────────────────────────────────────────────────────────────
-- TaglessProg smart constructors are simulation-compatible
-- ─────────────────────────────────────────────────────────────────────────────
--
-- For each smart constructor, we prove:
--   AlgSim R alg₁ alg₂ → R (constructor alg₁) (constructor alg₂)
--
-- This is the "free theorem" spelled out explicitly.

-- returnProg is simulation-compatible
sim-returnProg :
  ∀ {rank} {sched : _} {dom : Shape rank}
  {M₁ M₂ : Set → Set}
  {R : ∀ {A} → M₁ A → M₂ A → Set}
  {alg₁ : KernelAlg sched dom M₁}
  {alg₂ : KernelAlg sched dom M₂} →
  AlgSim R alg₁ alg₂ →
  ∀ {A} (a : A) →
  R (returnProg a alg₁) (returnProg a alg₂)
sim-returnProg sim a = sim-return sim a

-- bindProg is simulation-compatible given simulation-compatible sub-terms
sim-bindProg :
  ∀ {rank} {sched : _} {dom : Shape rank}
  {M₁ M₂ : Set → Set}
  {R : ∀ {A} → M₁ A → M₂ A → Set}
  (alg₁ : KernelAlg sched dom M₁)
  (alg₂ : KernelAlg sched dom M₂) →
  AlgSim R alg₁ alg₂ →
  ∀ {A B}
  (p  : TaglessProg sched dom A)
  (f  : A → TaglessProg sched dom B) →
  R (p alg₁) (p alg₂) →
  (∀ a → R (f a alg₁) (f a alg₂)) →
  R (bindProg p f alg₁) (bindProg p f alg₂)
sim-bindProg alg₁ alg₂ sim p f Rp Rf =
  sim-bind sim (p alg₁) (p alg₂) Rp (λ a → f a alg₁) (λ a → f a alg₂) Rf

-- forProg is simulation-compatible given simulation-compatible per-step bodies
sim-forProg :
  ∀ {rank} {sched : _} {dom : Shape rank}
  {M₁ M₂ : Set → Set}
  {R : ∀ {A} → M₁ A → M₂ A → Set}
  (alg₁ : KernelAlg sched dom M₁)
  (alg₂ : KernelAlg sched dom M₂) →
  AlgSim R alg₁ alg₂ →
  (body : Index sched dom → TaglessProg sched dom ⊤) →
  (∀ ix → R (body ix alg₁) (body ix alg₂)) →
  R (forProg body alg₁) (forProg body alg₂)
sim-forProg alg₁ alg₂ sim body Rbody =
  sim-for sim (λ ix → body ix alg₁) (λ ix → body ix alg₂) Rbody

-- ─────────────────────────────────────────────────────────────────────────────
-- Concrete instances for copy-body and inc-body
-- ─────────────────────────────────────────────────────────────────────────────

open import Loom.Theory.Examples using (input; output; board)

-- copy-body at any index is simulation-compatible
sim-copy-body :
  ∀ {M₁ M₂ : Set → Set}
  {R : ∀ {A} → M₁ A → M₂ A → Set}
  (alg₁ : KernelAlg rect board M₁)
  (alg₂ : KernelAlg rect board M₂) →
  AlgSim R alg₁ alg₂ →
  ∀ ix →
  R (copy-body ix alg₁) (copy-body ix alg₂)
sim-copy-body alg₁ alg₂ sim ix =
  sim-bind sim
    (alg-read alg₁ read-only (rectAccess input ix))
    (alg-read alg₂ read-only (rectAccess input ix))
    (sim-read sim read-only (rectAccess input ix))
    (λ v → alg-write alg₁ write-only (rectAccess output ix) v)
    (λ v → alg-write alg₂ write-only (rectAccess output ix) v)
    (λ v → sim-write sim write-only (rectAccess output ix) v)

-- inc-body at any index is simulation-compatible
sim-inc-body :
  ∀ {M₁ M₂ : Set → Set}
  {R : ∀ {A} → M₁ A → M₂ A → Set}
  (alg₁ : KernelAlg rect board M₁)
  (alg₂ : KernelAlg rect board M₂) →
  AlgSim R alg₁ alg₂ →
  ∀ ix →
  R (inc-body ix alg₁) (inc-body ix alg₂)
sim-inc-body alg₁ alg₂ sim ix =
  sim-bind sim
    (alg-read alg₁ read-only (rectAccess input ix))
    (alg-read alg₂ read-only (rectAccess input ix))
    (sim-read sim read-only (rectAccess input ix))
    (λ v → alg-write alg₁ write-only (rectAccess output ix) (suc v))
    (λ v → alg-write alg₂ write-only (rectAccess output ix) (suc v))
    (λ v → sim-write sim write-only (rectAccess output ix) (suc v))

