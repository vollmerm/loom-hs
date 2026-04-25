{-# OPTIONS --safe #-}

-- End-to-end parallel correctness for the tagless embedding.
--
-- This module connects the tagless sequential execution (runTaglessProg under
-- EnvAlg) to the formal parallel semantics (runParallel) for any valid
-- schedule, for any OutputInputConsistent ExactCoverRect2Kernel.
--
-- The central theorem is:
--
--   tagless-parallel-correct :
--     (kernel : ExactCoverRect2Kernel rows cols) →
--     OutputInputConsistent (base (wholeKernel kernel)) →
--     (vs : ValidSchedule (base (wholeKernel kernel)) n) →
--     (env : Env rank2) →
--     PostStateEq
--       (proj₁ (runTaglessProg (kernelProgram (base (wholeKernel kernel)))
--                              (EnvAlg (shape2 rows cols)) env))
--       (runParallel env (base (wholeKernel kernel)) vs)
--
-- The proof proceeds via a diamond through rect2Expected:
--
--   tagless (EnvAlg)        = rect2Expected   (tagless-correct)
--   runParallel (canonical) = rect2Expected   (runParallel-canonical-correct)
--   tagless (EnvAlg)        = runParallel (canonical)  (tagless-parallel-canonical)
--   runParallel (canonical) = runParallel (any vs)     (parallel-schedule-invariance + OIC)
--   ──────────────────────────────────────────────────────────────────────────
--   tagless (EnvAlg)        = runParallel (any vs)     (tagless-parallel-correct)

module Loom.Theory.Tagless.ParallelCorrect where

open import Data.Fin.Base       using (combine; remQuot)
open import Data.Fin.Properties using (remQuot-combine)
open import Data.Nat.Base       using (_*_)
import Loom.Theory.ExactCoverLinear        as ECL
import Loom.Theory.ExactCoverRect2         as ECR2
open import Loom.Theory.ParallelSemantics
  using (runParallel; parallel-schedule-invariance)
open import Loom.Theory.Pointwise
  using (kernelProgram; outputArr; inputArr; transform; inputAt)
open import Loom.Theory.Prelude
open import Loom.Theory.ScheduleIndependent
  using (OutputInputConsistent; ValidSchedule)
open import Loom.Theory.Semantics
  using (Env; PostStateEq; lookupEnv; resolve)
open import Loom.Theory.Shape              using (rank2; shape2; arrayEq)
open import Loom.Theory.Tagless.GeneralInterchange
  using (runTaglessProg; runTaglessProg-env-bridge)
open import Loom.Theory.Tagless.StateInterp using (EnvAlg)
import Loom.Theory.WholeLinear             as WL
open import Loom.Theory.WholeRect2         using (base)
open import Relation.Nullary              using (no; yes)

-- ─────────────────────────────────────────────────────────────────────────────
-- Lemma: tagless-correct
--
-- The tagless sequential execution (proj₁ of runTaglessProg under EnvAlg)
-- equals the rect2Expected specification.  The proof chains the env bridge
-- (runTaglessProg-env-bridge) and the runRect2 state characterisation
-- (ECR2.runRect2-state) through transitivity.
-- ─────────────────────────────────────────────────────────────────────────────

tagless-correct :
  ∀ {rows cols} →
  (kernel : ECR2.ExactCoverRect2Kernel rows cols) →
  (env : Env rank2) →
  PostStateEq
    (proj₁ (runTaglessProg
      (kernelProgram (base (ECR2.wholeKernel kernel)))
      (EnvAlg (shape2 rows cols))
      env))
    (ECR2.rect2Expected kernel env)
tagless-correct kernel env arr ix =
  trans
    (cong (λ e → e arr ix)
      (runTaglessProg-env-bridge
        (kernelProgram (base (ECR2.wholeKernel kernel)))
        env))
    (ECR2.runRect2-state kernel env arr ix)

-- ─────────────────────────────────────────────────────────────────────────────
-- Canonical schedule
--
-- Every ExactCoverRect2Kernel gives rise to a ValidSchedule via the row-major
-- flattening Fin rows × Fin cols ↔ Fin (rows * cols).  The steps and
-- outputUnique components are extracted from the WholeLinear layer via
-- toExactCoverKernel; the cover components come directly from ECL.
-- ─────────────────────────────────────────────────────────────────────────────

canonicalSchedule :
  ∀ {rows cols} →
  (kernel : ECR2.ExactCoverRect2Kernel rows cols) →
  ValidSchedule (base (ECR2.wholeKernel kernel)) (rows * cols)
canonicalSchedule kernel =
  let ecl = ECR2.toExactCoverKernel kernel
  in record
    { steps        = WL.WholeKernel.steps       (ECL.wholeKernel ecl)
    ; outputUnique = WL.WholeKernel.outputUnique (ECL.wholeKernel ecl)
    ; coverIndex   = ECL.coverIndex  ecl
    ; outputCovered = ECL.outputCovered ecl
    }

-- ─────────────────────────────────────────────────────────────────────────────
-- Lemma: runParallel-canonical-correct
--
-- Running canonicalSchedule in the parallel model yields rect2Expected.
-- Proof by case split on arrayEq:
--   no  branch: both sides are env arr ix (definitional).
--   yes branch: both sides compute to transform applied to inputAt, differing
--               only by remQuot cols (combine r c) vs (r , c); connected by
--               remQuot-combine.
-- ─────────────────────────────────────────────────────────────────────────────

runParallel-canonical-correct :
  ∀ {rows cols} →
  (kernel : ECR2.ExactCoverRect2Kernel rows cols) →
  (env : Env rank2) →
  PostStateEq
    (runParallel env (base (ECR2.wholeKernel kernel)) (canonicalSchedule kernel))
    (ECR2.rect2Expected kernel env)
runParallel-canonical-correct {rows} {cols} kernel env arr ix
  with arrayEq arr (outputArr (base (ECR2.wholeKernel kernel)))
... | no  _    = refl
... | yes refl =
  cong
    (λ p →
      transform (base (ECR2.wholeKernel kernel))
        (lookupEnv env
          (inputArr (base (ECR2.wholeKernel kernel)))
          (resolve (inputAt (base (ECR2.wholeKernel kernel)) p))))
    (remQuot-combine {n = rows} {k = cols}
      (ECR2.coverRow kernel ix)
      (ECR2.coverCol kernel ix))

-- ─────────────────────────────────────────────────────────────────────────────
-- Corollary: tagless-parallel-canonical
--
-- Tagless sequential execution equals parallel execution under the canonical
-- schedule.
-- ─────────────────────────────────────────────────────────────────────────────

tagless-parallel-canonical :
  ∀ {rows cols} →
  (kernel : ECR2.ExactCoverRect2Kernel rows cols) →
  (env : Env rank2) →
  PostStateEq
    (proj₁ (runTaglessProg
      (kernelProgram (base (ECR2.wholeKernel kernel)))
      (EnvAlg (shape2 rows cols))
      env))
    (runParallel env (base (ECR2.wholeKernel kernel)) (canonicalSchedule kernel))
tagless-parallel-canonical kernel env arr ix =
  trans
    (tagless-correct kernel env arr ix)
    (sym (runParallel-canonical-correct kernel env arr ix))

-- ─────────────────────────────────────────────────────────────────────────────
-- HEADLINE THEOREM: tagless-parallel-correct
--
-- For any ExactCoverRect2Kernel that is OutputInputConsistent, the tagless
-- sequential execution under EnvAlg produces the same post-state as parallel
-- execution under any valid schedule.
--
-- This is the formal justification for why parFor is correct: the library's
-- sequential tagless execution is equivalent to parallel execution whenever
-- the kernel satisfies the OutputInputConsistency condition (reads and writes
-- use different arrays, so all parallel iterations see the same input state).
-- ─────────────────────────────────────────────────────────────────────────────

tagless-parallel-correct :
  ∀ {rows cols n} →
  (kernel : ECR2.ExactCoverRect2Kernel rows cols) →
  OutputInputConsistent (base (ECR2.wholeKernel kernel)) →
  (vs : ValidSchedule (base (ECR2.wholeKernel kernel)) n) →
  (env : Env rank2) →
  PostStateEq
    (proj₁ (runTaglessProg
      (kernelProgram (base (ECR2.wholeKernel kernel)))
      (EnvAlg (shape2 rows cols))
      env))
    (runParallel env (base (ECR2.wholeKernel kernel)) vs)
tagless-parallel-correct kernel oi vs env arr ix =
  trans
    (tagless-parallel-canonical kernel env arr ix)
    (parallel-schedule-invariance
      (base (ECR2.wholeKernel kernel)) oi (canonicalSchedule kernel) vs env arr ix)
