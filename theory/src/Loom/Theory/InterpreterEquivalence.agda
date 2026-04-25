{-# OPTIONS --safe #-}

-- Interpreter Equivalence Theorem.
--
-- The headline result: for any KernelAlg that is *sound* (one whose post-state
-- for any ExactCoverRect2Kernel-derived program agrees pointwise with the
-- canonical row-major execution runRect2), running the tagless program under
-- that algebra is observationally equivalent to parallel execution under any
-- valid schedule, provided the kernel is OutputInputConsistent.
--
-- The soundness condition `SoundKernelAlg` is defined at the traversal level
-- — it says the algebra's output agrees with runRect2, not just that its
-- individual primitive operations agree.  Both `EnvAlg` (row-major) and
-- `InterchangeAlg` (column-major) satisfy this condition:
--
--   EnvAlg-sound     : SoundKernelAlg (EnvAlg dom)
--     by runTaglessProg-env-bridge (direct)
--
--   InterchangeAlg-sound : SoundKernelAlg (InterchangeAlg dom)
--     by runTaglessProg-interchange-bridge + loop-interchange-preserves-state
--
-- The headline theorem is `interpreter-equivalence`:
--
--   ∀ kernel oi vs alg sound env arr ix →
--     PostStateEq
--       (proj₁ (runTaglessProg (kernelProgram (base (wholeKernel kernel))) alg env))
--       (runParallel env (base (wholeKernel kernel)) vs)
--
-- Proof chain (for each (arr, ix)):
--
--   alg post-state arr ix
--     ≡ runRect2 env prog arr ix              (sound kernel env arr ix)
--     ≡ rect2Expected kernel env arr ix       (ECR2.runRect2-state)
--     ≡ runParallel env base canonical arr ix (sym PC.runParallel-canonical-correct)
--     ≡ runParallel env base vs arr ix        (parallel-schedule-invariance + OI)
--
-- Paper connection: this is the Agda mechanisation of the paper's one-sentence
-- thesis — "loop transformations are compositional interpreter swaps".
-- Any algebra that traverses an OI-consistent kernel with the same coverage
-- as the sequential spec (captured by SoundKernelAlg) produces a result that
-- is indistinguishable from the parallel semantics.  Crucially, EnvAlg and
-- InterchangeAlg are both sound: swapping one for the other — changing only
-- the traversal strategy encoded in alg-for — is a semantics-preserving
-- transformation.

module Loom.Theory.InterpreterEquivalence where

import Loom.Theory.ExactCoverRect2                  as ECR2
import Loom.Theory.Tagless.ParallelCorrect           as PC
open import Loom.Theory.LoopInterchange
  using (loop-interchange-preserves-state)
open import Loom.Theory.ParallelSemantics
  using (runParallel; parallel-schedule-invariance)
open import Loom.Theory.Pointwise          using (kernelProgram)
open import Loom.Theory.Prelude
open import Loom.Theory.RectExecution      using (runRect2; runRect2Interchange)
open import Loom.Theory.Schedule           using (rect)
open import Loom.Theory.ScheduleIndependent
  using (OutputInputConsistent; ValidSchedule)
open import Loom.Theory.Semantics
  using (Env; PostStateEq; lookupEnv)
open import Loom.Theory.Shape              using (rank2; shape2)
open import Loom.Theory.Syntax             using (Program)
open import Loom.Theory.Tagless.Algebra    using (KernelAlg)
open import Loom.Theory.Tagless.GeneralInterchange
  using (runTaglessProg; runTaglessProg-env-bridge; runTaglessProg-interchange-bridge)
open import Loom.Theory.Tagless.InterchangeInterp using (InterchangeAlg)
open import Loom.Theory.Tagless.StateInterp       using (StateM; EnvAlg)
open import Loom.Theory.WholeRect2         using (base)

-- ─────────────────────────────────────────────────────────────────────────────
-- SoundKernelAlg: traversal-level soundness condition
-- ─────────────────────────────────────────────────────────────────────────────
--
-- A KernelAlg is *sound* if, for every ExactCoverRect2Kernel-derived program,
-- the post-state produced by running the program under the algebra agrees
-- pointwise with the canonical row-major execution runRect2.
--
-- This condition is defined at the traversal level (post-state of the entire
-- program, not just the kernel body), because different algebras may encode
-- different traversal orders in their alg-for field, and a general algebra's
-- alg-for is abstract.  The condition captures exactly what is needed to
-- connect the tagless layer to the parallel semantics: any algebra that covers
-- the domain with the same result as the sequential spec is parallel-correct.
--
-- Both EnvAlg and InterchangeAlg satisfy this condition (see below).  The
-- condition is satisfied vacuously by any algebra definitionally equal to
-- EnvAlg, and non-trivially by InterchangeAlg via the loop-interchange theorem.

SoundKernelAlg :
  ∀ {rows cols} →
  KernelAlg rect (shape2 rows cols) (StateM rank2) →
  Set
SoundKernelAlg {rows} {cols} alg =
  ∀ (kernel : ECR2.ExactCoverRect2Kernel rows cols) →
  ∀ (env    : Env rank2) →
  PostStateEq
    (proj₁ (runTaglessProg
      (kernelProgram (base (ECR2.wholeKernel kernel)))
      alg env))
    (runRect2 env
      (kernelProgram (base (ECR2.wholeKernel kernel))))

-- ─────────────────────────────────────────────────────────────────────────────
-- EnvAlg-sound
-- ─────────────────────────────────────────────────────────────────────────────
--
-- The row-major sequential algebra is sound.  The proof is a direct application
-- of runTaglessProg-env-bridge: proj₁ (runTaglessProg prog (EnvAlg dom) env) is
-- definitionally equal to runRect2 env prog for any program, so the pointwise
-- equality follows immediately from congruence.

EnvAlg-sound :
  ∀ {rows cols} →
  SoundKernelAlg (EnvAlg (shape2 rows cols))
EnvAlg-sound kernel env arr ix =
  cong (λ e → e arr ix)
    (runTaglessProg-env-bridge
      (kernelProgram (base (ECR2.wholeKernel kernel)))
      env)

-- ─────────────────────────────────────────────────────────────────────────────
-- InterchangeAlg-sound
-- ─────────────────────────────────────────────────────────────────────────────
--
-- The column-major interchange algebra is also sound.  The proof chains:
--
--   proj₁ (runTaglessProg prog (InterchangeAlg dom) env) arr ix
--     ≡ runRect2Interchange env prog arr ix   (runTaglessProg-interchange-bridge)
--     ≡ runRect2 env prog arr ix              (sym loop-interchange-preserves-state)
--
-- The loop interchange step uses the fact that every ExactCoverRect2Kernel
-- satisfies the exact-cover conditions required by
-- loop-interchange-preserves-state.

InterchangeAlg-sound :
  ∀ {rows cols} →
  SoundKernelAlg (InterchangeAlg (shape2 rows cols))
InterchangeAlg-sound kernel env arr ix =
  trans
    (cong (λ e → e arr ix)
      (runTaglessProg-interchange-bridge
        (kernelProgram (base (ECR2.wholeKernel kernel)))
        env))
    (sym (loop-interchange-preserves-state kernel env arr ix))

-- ─────────────────────────────────────────────────────────────────────────────
-- HEADLINE THEOREM: interpreter-equivalence
-- ─────────────────────────────────────────────────────────────────────────────
--
-- For any sound KernelAlg, any OutputInputConsistent ExactCoverRect2Kernel, and
-- any valid schedule vs, the tagless sequential execution under the algebra is
-- pointwise equal to parallel execution under vs.
--
-- The proof chains four equalities (for each (arr, ix)):
--
--   (1) proj₁ (runTaglessProg prog alg env) arr ix
--         ≡ runRect2 env prog arr ix
--       by: sound kernel env arr ix
--
--   (2) runRect2 env prog arr ix
--         ≡ rect2Expected kernel env arr ix
--       by: ECR2.runRect2-state kernel env arr ix
--
--   (3) rect2Expected kernel env arr ix
--         ≡ runParallel env base canonical arr ix
--       by: sym (PC.runParallel-canonical-correct kernel env arr ix)
--
--   (4) runParallel env base canonical arr ix
--         ≡ runParallel env base vs arr ix
--       by: parallel-schedule-invariance base oi canonical vs env arr ix
--
-- where prog     = kernelProgram (base (ECR2.wholeKernel kernel))
--       base     = WholeRect2.base (ECR2.wholeKernel kernel)
--       canonical = PC.canonicalSchedule kernel
--
-- Paper reading: the algebra is the "loop transformation interpreter".
-- Changing alg-for (the traversal strategy) while keeping the program fixed
-- is exactly what it means to "swap the interpreter" in the finally-tagless
-- style.  This theorem says any sound swap is semantics-preserving for
-- OI-consistent kernels.

interpreter-equivalence :
  ∀ {rows cols n} →
  (kernel : ECR2.ExactCoverRect2Kernel rows cols) →
  OutputInputConsistent (base (ECR2.wholeKernel kernel)) →
  (vs   : ValidSchedule (base (ECR2.wholeKernel kernel)) n) →
  (alg  : KernelAlg rect (shape2 rows cols) (StateM rank2)) →
  SoundKernelAlg alg →
  (env  : Env rank2) →
  PostStateEq
    (proj₁ (runTaglessProg
      (kernelProgram (base (ECR2.wholeKernel kernel)))
      alg
      env))
    (runParallel env (base (ECR2.wholeKernel kernel)) vs)
interpreter-equivalence kernel oi vs alg sound env arr ix =
  trans
    (sound kernel env arr ix)
    (trans
      (ECR2.runRect2-state kernel env arr ix)
      (trans
        (sym (PC.runParallel-canonical-correct kernel env arr ix))
        (parallel-schedule-invariance
          (base (ECR2.wholeKernel kernel)) oi
          (PC.canonicalSchedule kernel) vs
          env arr ix)))

-- ─────────────────────────────────────────────────────────────────────────────
-- Corollary 1: sequential-parallel-equivalence
-- ─────────────────────────────────────────────────────────────────────────────
--
-- Specialise to alg = EnvAlg (the row-major sequential interpreter).  This
-- recovers tagless-parallel-correct from Tagless.ParallelCorrect as a
-- one-liner via interpreter-equivalence + EnvAlg-sound.

sequential-parallel-equivalence :
  ∀ {rows cols n} →
  (kernel : ECR2.ExactCoverRect2Kernel rows cols) →
  OutputInputConsistent (base (ECR2.wholeKernel kernel)) →
  (vs  : ValidSchedule (base (ECR2.wholeKernel kernel)) n) →
  (env : Env rank2) →
  PostStateEq
    (proj₁ (runTaglessProg
      (kernelProgram (base (ECR2.wholeKernel kernel)))
      (EnvAlg (shape2 _ _))
      env))
    (runParallel env (base (ECR2.wholeKernel kernel)) vs)
sequential-parallel-equivalence kernel oi vs env =
  interpreter-equivalence kernel oi vs (EnvAlg (shape2 _ _)) EnvAlg-sound env

-- ─────────────────────────────────────────────────────────────────────────────
-- Corollary 2: interchange-parallel-equivalence
-- ─────────────────────────────────────────────────────────────────────────────
--
-- Specialise to alg = InterchangeAlg (the column-major interchange interpreter).
-- This shows that the column-major interpreter is also observationally
-- equivalent to the parallel semantics.  Together with Corollary 1, it
-- establishes that row-major, column-major, and parallel executions all agree
-- on every output cell of any OI-consistent kernel — the column-major
-- transformation is a zero-cost, semantics-preserving interpreter swap.

interchange-parallel-equivalence :
  ∀ {rows cols n} →
  (kernel : ECR2.ExactCoverRect2Kernel rows cols) →
  OutputInputConsistent (base (ECR2.wholeKernel kernel)) →
  (vs  : ValidSchedule (base (ECR2.wholeKernel kernel)) n) →
  (env : Env rank2) →
  PostStateEq
    (proj₁ (runTaglessProg
      (kernelProgram (base (ECR2.wholeKernel kernel)))
      (InterchangeAlg (shape2 _ _))
      env))
    (runParallel env (base (ECR2.wholeKernel kernel)) vs)
interchange-parallel-equivalence kernel oi vs env =
  interpreter-equivalence kernel oi vs (InterchangeAlg (shape2 _ _)) InterchangeAlg-sound env

-- ─────────────────────────────────────────────────────────────────────────────
-- Corollary 3: sound-algebras-agree
-- ─────────────────────────────────────────────────────────────────────────────
--
-- Any two sound KernelAlg instances produce observationally equivalent
-- post-states on any OI-consistent ExactCoverRect2Kernel.
--
-- This is the "compositionality" corollary: the set of sound algebras forms an
-- equivalence class under PostStateEq.  If alg₁ is sound and alg₂ is sound,
-- the interpreter substitution alg₁ ↦ alg₂ (or any finite chain of such swaps)
-- is semantics-preserving — no new correctness proof is needed per pair.
--
-- This is the mechanised form of the paper's one-sentence thesis:
--   "loop transformations are expressed as compositional interpreter swaps."
-- Any traversal strategy proven sound can be freely exchanged with any other
-- sound strategy; equivalence to all current and future sound algebras and to
-- the parallel semantics follows automatically from this corollary.
--
-- Proof: apply interpreter-equivalence to each algebra, then chain the two
-- post-state equalities by trans and sym.
--
--   proj₁ (runTaglessProg prog alg₁ env) arr ix
--     ≡ runParallel env base vs arr ix        (interpreter-equivalence for alg₁)
--     ≡ proj₁ (runTaglessProg prog alg₂ env) arr ix
--                                             (sym (interpreter-equivalence for alg₂))

sound-algebras-agree :
  ∀ {rows cols n} →
  (kernel : ECR2.ExactCoverRect2Kernel rows cols) →
  OutputInputConsistent (base (ECR2.wholeKernel kernel)) →
  (vs   : ValidSchedule (base (ECR2.wholeKernel kernel)) n) →
  (alg₁ alg₂ : KernelAlg rect (shape2 rows cols) (StateM rank2)) →
  SoundKernelAlg alg₁ →
  SoundKernelAlg alg₂ →
  (env  : Env rank2) →
  PostStateEq
    (proj₁ (runTaglessProg
      (kernelProgram (base (ECR2.wholeKernel kernel)))
      alg₁ env))
    (proj₁ (runTaglessProg
      (kernelProgram (base (ECR2.wholeKernel kernel)))
      alg₂ env))
sound-algebras-agree kernel oi vs alg₁ alg₂ sound₁ sound₂ env arr ix =
  trans
    (interpreter-equivalence kernel oi vs alg₁ sound₁ env arr ix)
    (sym (interpreter-equivalence kernel oi vs alg₂ sound₂ env arr ix))
