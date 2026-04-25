{-# OPTIONS --safe #-}

-- General tagless interchange theorem.
--
-- Generalises Tagless.InterchangeTheorem.copy-tagless-interchange from the
-- concrete copy/inc programs on a fixed 8×8 board to any program derived from
-- an ExactCoverRect2Kernel.
--
-- The central definition is runTaglessProg, the canonical way to lift any deep
-- Program into the tagless layer:
--
--   runTaglessProg : Program rect (shape2 rows cols) → TaglessProg rect (shape2 rows cols) ⊤
--   runTaglessProg prog = forProg (toTagless prog)
--
-- Two bridge lemmas connect this to the deep-embedding runners:
--
--   proj₁ (runTaglessProg prog (EnvAlg dom) env)         ≡ runRect2 env prog
--   proj₁ (runTaglessProg prog (InterchangeAlg dom) env) ≡ runRect2Interchange env prog
--
-- Each bridge is proved by foldFin-cong applied to the kernel-level agreement
-- lemma (toTaglessKernel-EnvAlg-agree), which establishes by structural
-- induction on the Kernel GADT that EnvAlg's interpretation of any kernel body
-- agrees with evalKernel.  InterchangeAlg shares the same result because Kernel
-- terms never invoke alg-for, and alg-for is the only field that differs
-- between the two algebras.
--
-- Together with loop-interchange-preserves-state the bridges yield:
--
--   tagless-interchange-general :
--     (kernel : ExactCoverRect2Kernel rows cols) →
--     (env : Env rank2) →
--     PostStateEq
--       (proj₁ (runTaglessProg prog (EnvAlg dom) env))
--       (proj₁ (runTaglessProg prog (InterchangeAlg dom) env))
--     where prog = kernelProgram (base (wholeKernel kernel))
--           dom  = shape2 rows cols

module Loom.Theory.Tagless.GeneralInterchange where

open import Loom.Theory.ExactCoverRect2 using (ExactCoverRect2Kernel; wholeKernel)
open import Loom.Theory.LoopInterchange  using (loop-interchange-preserves-state)
open import Loom.Theory.Pointwise        using (kernelProgram)
open import Loom.Theory.Prelude
open import Loom.Theory.RectExecution   using (runRect2; runRect2Interchange)
open import Loom.Theory.Schedule        using (rect)
open import Loom.Theory.Semantics       using (Env; lookupEnv; PostStateEq;
                                                evalKernel; readStore; runAt)
open import Loom.Theory.Shape           using (Rank; Shape; rank2; shape2)
open import Loom.Theory.Syntax          using (Program; body; Kernel)
import      Loom.Theory.Syntax as Syn
open import Loom.Theory.Tagless.BridgeToDeep      using (toTagless; toTaglessKernel)
open import Loom.Theory.Tagless.InterchangeInterp using (InterchangeAlg)
open import Loom.Theory.Tagless.Program           using (TaglessProg; forProg)
open import Loom.Theory.Tagless.StateInterp       using (EnvAlg)
open import Loom.Theory.Traversal       using (foldFin; foldFin-cong)
open import Loom.Theory.WholeRect2      using (base)

-- ─────────────────────────────────────────────────────────────────────────────
-- General tagless program runner
-- ─────────────────────────────────────────────────────────────────────────────
--
-- runTaglessProg lifts any deep Program into the tagless layer by combining
-- forProg with toTagless.  The concrete copy-prog / inc-prog in
-- Tagless.Program are special cases of this combinator.

runTaglessProg :
  ∀ {rows cols} →
  Program rect (shape2 rows cols) →
  TaglessProg rect (shape2 rows cols) ⊤
runTaglessProg prog = forProg (toTagless prog)

-- ─────────────────────────────────────────────────────────────────────────────
-- Kernel-level agreement lemma (private)
-- ─────────────────────────────────────────────────────────────────────────────
--
-- For any Kernel k, running it through toTaglessKernel under EnvAlg produces
-- the same state-transformer as evalKernel.  The proof is by structural
-- induction on k:
--   pure  : refl
--   write : refl
--   read  : reduction to (next (readStore …)) on both sides, then IH
--   >>=   : with-clause to match evalKernel on m, unify via refl, then IH on f
--
-- The dom argument does not affect the result for Kernel terms (alg-for is
-- never called), so it remains implicit.
--
-- InterchangeAlg shares the same fields as EnvAlg except for alg-for.
-- Because alg-for is never invoked when interpreting a Kernel (which has no
-- for-loop constructor), toTaglessKernel (InterchangeAlg dom) k env is
-- definitionally equal to toTaglessKernel (EnvAlg dom) k env.  Therefore the
-- same lemma covers both algebra instances.

private
  -- Agreement for EnvAlg
  toTaglessKernel-EnvAlg-agree :
    ∀ {rank ty} {dom : Shape rank} →
    (k   : Kernel {rank} rect ty) →
    (env : Env rank) →
    toTaglessKernel (EnvAlg dom) k env ≡ evalKernel env k
  toTaglessKernel-EnvAlg-agree (Syn.pure val) env = refl
  toTaglessKernel-EnvAlg-agree (Syn.write writable access value) env = refl
  toTaglessKernel-EnvAlg-agree (Syn.read {arr = arr} readable access next) env =
    toTaglessKernel-EnvAlg-agree (next (readStore (env arr) access)) env
  toTaglessKernel-EnvAlg-agree (m Syn.>>= f) env
    with evalKernel env m | toTaglessKernel-EnvAlg-agree m env
  ... | env' , a | refl = toTaglessKernel-EnvAlg-agree (f a) env'

  -- Agreement for InterchangeAlg — identical proof, separate so Agda does not
  -- try to unify EnvAlg dom with InterchangeAlg dom when building the bridge.
  toTaglessKernel-InterchangeAlg-agree :
    ∀ {rank ty} {dom : Shape rank} →
    (k   : Kernel {rank} rect ty) →
    (env : Env rank) →
    toTaglessKernel (InterchangeAlg dom) k env ≡ evalKernel env k
  toTaglessKernel-InterchangeAlg-agree (Syn.pure val) env = refl
  toTaglessKernel-InterchangeAlg-agree (Syn.write writable access value) env = refl
  toTaglessKernel-InterchangeAlg-agree (Syn.read {arr = arr} readable access next) env =
    toTaglessKernel-InterchangeAlg-agree (next (readStore (env arr) access)) env
  toTaglessKernel-InterchangeAlg-agree (m Syn.>>= f) env
    with evalKernel env m | toTaglessKernel-InterchangeAlg-agree m env
  ... | env' , a | refl = toTaglessKernel-InterchangeAlg-agree (f a) env'

-- ─────────────────────────────────────────────────────────────────────────────
-- Bridge lemmas
-- ─────────────────────────────────────────────────────────────────────────────
--
-- Each bridge connects runTaglessProg under one algebra to the corresponding
-- deep-embedding runner.  The proofs use foldFin-cong to push the pointwise
-- agreement (via cong proj₁ of toTaglessKernel-EnvAlg-agree) through the
-- nested outer/inner foldFin structure.

runTaglessProg-env-bridge :
  ∀ {rows cols} →
  (prog : Program rect (shape2 rows cols)) →
  (env  : Env rank2) →
  proj₁ (runTaglessProg prog (EnvAlg (shape2 rows cols)) env) ≡
  runRect2 env prog
runTaglessProg-env-bridge prog env =
  foldFin-cong
    (λ row env' →
      foldFin-cong
        (λ col env'' →
          cong proj₁ (toTaglessKernel-EnvAlg-agree (body prog (row , col)) env''))
        env')
    env

runTaglessProg-interchange-bridge :
  ∀ {rows cols} →
  (prog : Program rect (shape2 rows cols)) →
  (env  : Env rank2) →
  proj₁ (runTaglessProg prog (InterchangeAlg (shape2 rows cols)) env) ≡
  runRect2Interchange env prog
runTaglessProg-interchange-bridge prog env =
  foldFin-cong
    (λ col env' →
      foldFin-cong
        (λ row env'' →
          cong proj₁ (toTaglessKernel-InterchangeAlg-agree (body prog (row , col)) env''))
        env')
    env

-- ─────────────────────────────────────────────────────────────────────────────
-- General interchange theorem
-- ─────────────────────────────────────────────────────────────────────────────
--
-- For any program derived from an ExactCoverRect2Kernel, running it under the
-- row-major interpreter (EnvAlg) and the column-major interpreter
-- (InterchangeAlg) produces identical post-states.
--
-- The proof chain is:
--   tagless/EnvAlg  ≡  runRect2               (by runTaglessProg-env-bridge)
--                   ≡  runRect2Interchange    (by loop-interchange-preserves-state)
--                   ≡  tagless/InterchangeAlg (by sym runTaglessProg-interchange-bridge)
--
-- This is the general version of copy-tagless-interchange: the program need
-- not be the copy kernel; any ExactCoverRect2Kernel suffices.

tagless-interchange-general :
  ∀ {rows cols} →
  (kernel : ExactCoverRect2Kernel rows cols) →
  (env : Env rank2) →
  PostStateEq
    (proj₁ (runTaglessProg (kernelProgram (base (wholeKernel kernel)))
                           (EnvAlg (shape2 rows cols)) env))
    (proj₁ (runTaglessProg (kernelProgram (base (wholeKernel kernel)))
                           (InterchangeAlg (shape2 rows cols)) env))
tagless-interchange-general kernel env arr ix =
  let prog = kernelProgram (base (wholeKernel kernel))
  in
  trans
    (cong (λ e → lookupEnv e arr ix)
      (runTaglessProg-env-bridge prog env))
    (trans
      (loop-interchange-preserves-state kernel env arr ix)
      (cong (λ e → lookupEnv e arr ix)
        (sym (runTaglessProg-interchange-bridge prog env))))
