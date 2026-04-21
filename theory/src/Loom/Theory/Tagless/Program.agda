{-# OPTIONS --safe #-}

-- TaglessProg: programs parametric over a KernelAlg instance.
--
-- This is the Agda counterpart of Haskell's Prog type:
--
--   newtype Prog a = Prog { unProg :: ∀ repr r. Loop repr => (a → repr r) → repr r }
--
-- Here we use a simpler direct-style rather than CPS, since Agda's type system
-- handles the ∀ {repr} quantifier without the continuation trick needed in
-- Haskell to recover a monad instance.
--
-- TaglessProg A lives in Set₁ because it universally quantifies over
-- repr : Set → Set.
--
-- Smart constructors mirror the Haskell Prog primitives:
--   readProg  ↔  loopReadArr
--   writeProg ↔  loopWriteArr
--   forProg   ↔  loopParFor / parFor
--   returnProg ↔  pure
--   bindProg  ↔  >>=
--
-- Two concrete example programs (copy-body and inc-body) correspond to the
-- loop bodies of the existing Pointwise kernels, giving a direct link between
-- the tagless layer and the deep-embedding Examples.

module Loom.Theory.Tagless.Program where

open import Loom.Theory.Prelude
open import Loom.Theory.Access using (CanRead; CanWrite; Access; read-only; write-only)
open import Loom.Theory.Index using (Index)
open import Loom.Theory.Schedule using (Schedule)
open import Loom.Theory.Shape using (Rank; Shape)
open import Loom.Theory.Tagless.Algebra

-- ─────────────────────────────────────────────────────────────────────────────
-- The TaglessProg type
-- ─────────────────────────────────────────────────────────────────────────────

-- A program of type A parametric over any KernelAlg for schedule sched / domain dom.
-- This lives in Set₁.
TaglessProg :
  ∀ {rank : Rank} →
  (sched : Schedule rank) →
  (dom   : Shape rank) →
  (A     : Set) →
  Set₁
TaglessProg sched dom A =
  ∀ {repr : Set → Set} →
  KernelAlg sched dom repr →
  repr A

-- ─────────────────────────────────────────────────────────────────────────────
-- Smart constructors (primitive programs)
-- ─────────────────────────────────────────────────────────────────────────────

returnProg :
  ∀ {rank sched dom A} →
  A → TaglessProg {rank} sched dom A
returnProg a alg = alg-return alg a

bindProg :
  ∀ {rank sched dom A B} →
  TaglessProg {rank} sched dom A →
  (A → TaglessProg {rank} sched dom B) →
  TaglessProg {rank} sched dom B
bindProg m f alg = alg-bind alg (m alg) (λ a → f a alg)

-- Read via a capability-indexed access witness
readProg :
  ∀ {rank sched dom cap arr} →
  CanRead cap →
  Access {rank} cap sched arr →
  TaglessProg sched dom ℕ
readProg readable access alg = alg-read alg readable access

-- Write via a capability-indexed access witness
writeProg :
  ∀ {rank sched dom cap arr} →
  CanWrite cap →
  Access {rank} cap sched arr →
  ℕ →
  TaglessProg sched dom ⊤
writeProg writable access value alg = alg-write alg writable access value

-- Run the for-loop over all domain indices
forProg :
  ∀ {rank sched dom} →
  (Index sched dom → TaglessProg {rank} sched dom ⊤) →
  TaglessProg sched dom ⊤
forProg body alg = alg-for alg (λ ix → body ix alg)

-- ─────────────────────────────────────────────────────────────────────────────
-- Monad laws (propositional)
-- ─────────────────────────────────────────────────────────────────────────────
--
-- These state that TaglessProg forms a monad *given* that the underlying
-- algebra satisfies the monad laws.  We express this as: if the algebra's
-- bind/return satisfy the law, so does the derived TaglessProg operation.
--
-- In the concrete instances (StateInterp) the laws hold definitionally.

-- Left identity: return a >>= f ≡ f a
tagless-left-id :
  ∀ {rank sched dom A B}
    {repr : Set → Set}
    (alg  : KernelAlg {rank} sched dom repr) →
  -- algebra must satisfy left-identity for its monad
  (law : ∀ {A B} (a : A) (f : A → repr B) → alg-bind alg (alg-return alg a) f ≡ f a) →
  (a   : A)
  (f   : A → TaglessProg sched dom B) →
  alg-bind alg (returnProg a alg) (λ x → f x alg) ≡ f a alg
tagless-left-id alg law a f = law a (λ x → f x alg)

-- Right identity: m >>= return ≡ m
tagless-right-id :
  ∀ {rank sched dom A}
    {repr : Set → Set}
    (alg  : KernelAlg {rank} sched dom repr) →
  (law : ∀ {A} (m : repr A) → alg-bind alg m (alg-return alg) ≡ m) →
  (m : TaglessProg sched dom A) →
  alg-bind alg (m alg) (alg-return alg) ≡ m alg
tagless-right-id alg law m = law (m alg)

-- Associativity: (m >>= f) >>= g ≡ m >>= (λ x → f x >>= g)
tagless-assoc :
  ∀ {rank sched dom A B C}
    {repr : Set → Set}
    (alg  : KernelAlg {rank} sched dom repr) →
  (law : ∀ {A B C} (m : repr A) (f : A → repr B) (g : B → repr C) →
    alg-bind alg (alg-bind alg m f) g ≡
    alg-bind alg m (λ x → alg-bind alg (f x) g)) →
  (m : TaglessProg sched dom A)
  (f : A → TaglessProg sched dom B)
  (g : B → TaglessProg sched dom C) →
  alg-bind alg (alg-bind alg (m alg) (λ x → f x alg)) (λ y → g y alg) ≡
  alg-bind alg (m alg) (λ x → alg-bind alg (f x alg) (λ y → g y alg))
tagless-assoc alg law m f g = law (m alg) (λ x → f x alg) (λ y → g y alg)

-- ─────────────────────────────────────────────────────────────────────────────
-- Concrete example programs
-- ─────────────────────────────────────────────────────────────────────────────
--
-- These mirror the `rect-copy` and `rect-inc` programs from Examples.agda,
-- but expressed as TaglessProg rather than as deep Syntax.Kernel programs.
-- They show that the tagless and deep-embedding styles describe the same
-- computations.

open import Loom.Theory.Access using (rectAccess)
open import Loom.Theory.Index using (RectIx)
open import Loom.Theory.Schedule using (rect)
open import Loom.Theory.Shape using (shape2)
open import Loom.Theory.Examples using (input; output; board)

-- Tagless copy body: read from input at ix, write to output at ix
copy-body :
  (ix : RectIx board) →
  TaglessProg rect board ⊤
copy-body ix =
  bindProg
    (readProg read-only (rectAccess input ix))
    (λ value → writeProg write-only (rectAccess output ix) value)

-- Tagless increment body: read from input at ix, write suc to output at ix
inc-body :
  (ix : RectIx board) →
  TaglessProg rect board ⊤
inc-body ix =
  bindProg
    (readProg read-only (rectAccess input ix))
    (λ value → writeProg write-only (rectAccess output ix) (suc value))

-- Full copy kernel: for-loop over the domain, running copy-body at each index
copy-prog : TaglessProg rect board ⊤
copy-prog = forProg copy-body

-- Full increment kernel
inc-prog : TaglessProg rect board ⊤
inc-prog = forProg inc-body
