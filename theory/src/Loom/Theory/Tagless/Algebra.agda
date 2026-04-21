{-# OPTIONS --safe #-}

-- The abstract loop algebra and its simulation relation.
--
-- KernelAlg repr is the Agda counterpart of the Haskell Loop typeclass:
--
--   class Monad repr => Loop repr where
--     loopParFor  :: Int -> (Int -> repr ()) -> repr ()
--     loopReadArr :: Arr a -> Int -> repr a
--     loopWriteArr :: Arr a -> Int -> a -> repr ()
--     ...
--
-- A TaglessProg (defined in Tagless.Program) is then:
--
--   type TaglessProg A = ∀ {repr : Set → Set} → KernelAlg repr → repr A
--
-- which mirrors Haskell's:
--
--   newtype Prog a = Prog { unProg :: ∀ repr r. Loop repr => (a → repr r) → repr r }
--
-- AlgSim R alg₁ alg₂ expresses that two algebra instances are behaviourally
-- related by R: every operation of alg₁ produces an alg₂-result that is R-related
-- to the alg₁-result.  This is the explicit substitute for the parametricity /
-- free-theorem argument that holds automatically in System F but must be proven
-- by hand in Agda.

module Loom.Theory.Tagless.Algebra where

open import Loom.Theory.Prelude
open import Loom.Theory.Semantics using (Env; lookupEnv; updateEnv; resolve)
open import Loom.Theory.Shape using (Rank; Shape; Array; shape)
open import Loom.Theory.Index using (Index; RectIx)
open import Loom.Theory.Access using (Capability; Access; ReadOnly; WriteOnly;
                                       CanRead; CanWrite; read-only; write-only)
open import Loom.Theory.Schedule using (Schedule)

-- ─────────────────────────────────────────────────────────────────────────────
-- The abstract kernel algebra
-- ─────────────────────────────────────────────────────────────────────────────

-- KernelAlg repr packages the monad operations together with the primitive
-- array-level operations that a loop body needs.  repr plays the role of the
-- Haskell `repr` type variable in `Loop repr`.
--
-- We keep the array operations rank-polymorphic via the existing
-- Array / Access / Index infrastructure rather than fixing a rank here.
-- The schedule index `sched` and domain `dom` are fixed per algebra instance
-- so that the for-loop can enumerate the right set of indices.

record KernelAlg
    {rank  : Rank}
    (sched : Schedule rank)
    (dom   : Shape rank)
    (repr  : Set → Set)
    : Set₁ where
  field
    -- Monad structure
    alg-return : ∀ {A} → A → repr A
    alg-bind   : ∀ {A B} → repr A → (A → repr B) → repr B

    -- Array read: read the value at a resolved rectangular index
    alg-read  :
      ∀ {cap arr} →
      CanRead cap →
      Access {rank} cap sched arr →
      repr ℕ

    -- Array write: write a value to a resolved rectangular index
    alg-write :
      ∀ {cap arr} →
      CanWrite cap →
      Access {rank} cap sched arr →
      ℕ →
      repr ⊤

    -- For-loop: enumerate all indices in the domain in some order
    alg-for : (Index sched dom → repr ⊤) → repr ⊤

open KernelAlg public

-- Derived monadic sequencing (>> analogue)
alg-seq :
  ∀ {rank} {sched : Schedule rank} {dom : Shape rank} {repr : Set → Set} →
  KernelAlg sched dom repr →
  ∀ {A B} →
  repr A → repr B → repr B
alg-seq alg m₁ m₂ = alg-bind alg m₁ (λ _ → m₂)

-- ─────────────────────────────────────────────────────────────────────────────
-- Algebra simulation relation
-- ─────────────────────────────────────────────────────────────────────────────

-- AlgSim R alg₁ alg₂ states that every primitive operation of alg₁ produces
-- a result that is R-related to the corresponding result of alg₂.
--
-- The `sim-for` field is the key one: it relates the for-loop results.
-- For the interchange theorem, both algebras traverse the same domain but in
-- different orders; `sim-for` captures that they still produce R-related
-- post-states.
--
-- This record deliberately does NOT claim R is a congruence or an equivalence;
-- callers supply whatever relation is appropriate.

record AlgSim
    {rank  : Rank}
    {sched : Schedule rank}
    {dom   : Shape rank}
    {M₁ M₂ : Set → Set}
    (R    : ∀ {A} → M₁ A → M₂ A → Set)
    (alg₁ : KernelAlg sched dom M₁)
    (alg₂ : KernelAlg sched dom M₂)
    : Set₁ where
  field
    -- return is R-related pointwise
    sim-return :
      ∀ {A} (a : A) →
      R (alg-return alg₁ a) (alg-return alg₂ a)

    -- bind preserves R given R-related arguments and R-related continuations
    sim-bind :
      ∀ {A B}
      (m₁ : M₁ A) (m₂ : M₂ A) →
      R m₁ m₂ →
      (f₁ : A → M₁ B) (f₂ : A → M₂ B) →
      (∀ a → R (f₁ a) (f₂ a)) →
      R (alg-bind alg₁ m₁ f₁) (alg-bind alg₂ m₂ f₂)

    -- reads are R-related for the same access
    sim-read :
      ∀ {cap arr}
      (readable : CanRead cap)
      (access   : Access {rank} cap sched arr) →
      R (alg-read alg₁ readable access)
        (alg-read alg₂ readable access)

    -- writes are R-related for the same access and value
    sim-write :
      ∀ {cap arr}
      (writable : CanWrite cap)
      (access   : Access {rank} cap sched arr)
      (value    : ℕ) →
      R (alg-write alg₁ writable access value)
        (alg-write alg₂ writable access value)

    -- for-loops are R-related given R-related per-step bodies
    sim-for :
      (body₁ : Index sched dom → M₁ ⊤)
      (body₂ : Index sched dom → M₂ ⊤) →
      (∀ ix → R (body₁ ix) (body₂ ix)) →
      R (alg-for alg₁ body₁) (alg-for alg₂ body₂)

open AlgSim public
