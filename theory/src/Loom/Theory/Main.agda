{-# OPTIONS --safe #-}

module Loom.Theory.Main where

open import Loom.Theory.Access
open import Loom.Theory.Determinism
open import Loom.Theory.ObservationalEquivalence
open import Loom.Theory.Examples
open import Loom.Theory.ExactCoverLinear
open import Loom.Theory.ExactCoverRect1
open import Loom.Theory.ExactCoverRect2
open import Loom.Theory.ExactCoverTiled using () renaming (wholeKernel to tiledWholeKernel)
open import Loom.Theory.ExactCoverTiled
open import Loom.Theory.Index
open import Loom.Theory.Prelude
open import Loom.Theory.FullRunTheorems
open import Loom.Theory.Pointwise
open import Loom.Theory.ProgramTheorems
open import Loom.Theory.RectExecution
open import Loom.Theory.LoopInterchange
open import Loom.Theory.Phase
open import Loom.Theory.PhaseSemantics
open import Loom.Theory.PhaseTheorems
open import Loom.Theory.Reduction
open import Loom.Theory.ReductionTheorems
open import Loom.Theory.Safety
open import Loom.Theory.SchedulePreservation
open import Loom.Theory.Schedule
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.KernelIndependence
open import Loom.Theory.ParallelSemantics
open import Loom.Theory.ConfluenceReduction
open import Function.Base using (_∘_)
open import Loom.Theory.PolyhedralModel
open import Loom.Theory.ScheduleEquivalence
open import Loom.Theory.ScheduleIndependent
open import Loom.Theory.StripMineTiling
open import Loom.Theory.Syntax
open import Loom.Theory.Traversal
open import Loom.Theory.Traversal.Export
open import Loom.Theory.TiledPointwise
open import Loom.Theory.WholeLinear
open import Loom.Theory.WholeTiled
open import Loom.Theory.WholeRect1
open import Loom.Theory.WholeRect2

-- Tagless / interpreter-composition layer
open import Loom.Theory.Tagless.Algebra
open import Loom.Theory.Tagless.Program
open import Loom.Theory.Tagless.StateInterp
open import Loom.Theory.Tagless.InterchangeInterp
open import Loom.Theory.Tagless.AlgSimulation
open import Loom.Theory.Tagless.InterchangeTheorem
open import Loom.Theory.Tagless.BridgeToDeep

two : ℕ
two = suc (suc zero)

two-plus-two : two + two ≡ suc (suc (suc (suc zero)))
two-plus-two = refl

rect-capability : Capability
rect-capability = ReadOnly

rect-copy-demo : lookupEnv (runAt initial₂ rect-copy zero₂) output zero₂ ≡ 7
rect-copy-demo = rect-copy-zero

tiled-copy-demo : lookupEnv (runAt initial₂ tiled-copy tile-zero) output zero₂ ≡ 7
tiled-copy-demo = tiled-copy-zero

line-copy-demo : lookupEnv (runRect1 line-initial line-rect-copy) line-output line-2 ≡ 4
line-copy-demo = line-output-2

line-inc-demo : lookupEnv (runAt line-initial line-rect-inc line-0) line-inc-output line-0 ≡ 5
line-inc-demo = line-inc-zero

tiny-rect-full-demo : lookupEnv (runRect2 tiny-initial₂ tiny-rect-copy) tiny-output tiny-zero₂ ≡ 5
tiny-rect-full-demo = tiny-rect-copy-full-zero

tiny-rect-full-demo-11 : lookupEnv (runRect2 tiny-initial₂ tiny-rect-copy) tiny-output tiny-11 ≡ 5
tiny-rect-full-demo-11 = tiny-output-11

tiny-inc-full-demo-11 : lookupEnv (runRect2 tiny-initial₂ tiny-rect-inc) tiny-inc-output tiny-11 ≡ 6
tiny-inc-full-demo-11 = tiny-inc-output-11

tiny-copy-loop-interchange-demo :
  lookupEnv (runRect2 tiny-initial₂ tiny-rect-copy) tiny-output tiny-11 ≡
    lookupEnv (runRect2Interchange tiny-initial₂ tiny-rect-copy) tiny-output tiny-11
tiny-copy-loop-interchange-demo = tiny-copy-loop-interchange-11

tiny-stripmine-demo :
  lookupEnv (runRect2 tiny-initial₂ tiny-rect-copy) tiny-output tiny-11 ≡
    lookupEnv (runTiles tiny-initial₂ (tiledWholeKernel tiny-stripmine-exact-kernel)) tiny-output tiny-11
tiny-stripmine-demo = tiny-stripmine-copy-11

line-sum-demo : foldArray1D sumReducer line-initial line-input ≡ 12
line-sum-demo = line-sum-is-12

line-inc-sum-demo : foldArray1D sumReducer (runRect1 line-initial line-rect-inc) line-inc-output ≡ 15
line-inc-sum-demo = line-inc-sum-is-15

line-phased-demo : lookupEnv (runPhases line-initial line-copy-then-inc) line-inc-output line-2 ≡ 5
line-phased-demo = line-phased-output-2

-- Observational equivalence: the line-copy kernel satisfies obs-correct.
line-copy-obs-demo :
  PostStateEq
    (run line-initial line-copy-wfk)
    (pointwiseSpec line-copy-wfk line-initial)
line-copy-obs-demo = line-copy-obs-correct

-- The output at index 2 equals 4, witnessed via the specification.
line-copy-obs-at-2-demo :
  lookupEnv (run line-initial line-copy-wfk) line-output line-2 ≡ 4
line-copy-obs-at-2-demo = line-copy-output-at-2

-- Polyhedral model: forward and backward orderings of line-copy are equivalent.
forward-backward-demo :
  PostStateEq
    (runWithSchedule line-initial line-copy-kernel forward-schedule)
    (runWithSchedule line-initial line-copy-kernel backward-schedule)
forward-backward-demo = forward-backward-equivalent line-initial

-- Schedule equivalence: any two valid schedules for an OutputInputConsistent
-- kernel produce the same post-state.
line-copy-schedule-equiv-demo :
  (vs1 vs2 : ValidSchedule line-copy-kernel 3) →
  PostStateEq
    (runWithSchedule line-initial line-copy-kernel vs1)
    (runWithSchedule line-initial line-copy-kernel vs2)
line-copy-schedule-equiv-demo vs1 vs2 =
  schedule-equivalence line-copy-kernel line-copy-oi-consistent vs1 vs2 line-initial

-- ────────────────────────────────────────────────────────────────────
-- Affine / factoring OI-consistency demo
-- (new in this session)
-- ────────────────────────────────────────────────────────────────────

-- `line-copy-oi-by-factoring` shows that OI-consistency can be derived
-- automatically when the input-access function factors through the
-- output-access function via id.  This avoids reasoning about the
-- pointwise kernel internals by applying the general theorem.
_ : OutputInputConsistent line-copy-kernel
_ = line-copy-oi-by-factoring

-- ────────────────────────────────────────────────────────────────────
-- Kernel independence demo
-- (new in this session)
-- ────────────────────────────────────────────────────────────────────

-- `line-exact-kernel` writes to array-6, reads from array-5.
-- `line-inc-exact-kernel` writes to array-7, reads from array-5.
-- All three non-overlap hypotheses discharge as λ () since the array
-- identifiers are distinct by construction.
line-copy-inc-independent :
  ∀ (env : Env rank1) →
  PostStateEq
    (runWhole (runWhole env (wholeKernel (Loom.Theory.ExactCoverRect1.toExactCoverKernel line-exact-kernel)))
              (wholeKernel (Loom.Theory.ExactCoverRect1.toExactCoverKernel line-inc-exact-kernel)))
    (runWhole (runWhole env (wholeKernel (Loom.Theory.ExactCoverRect1.toExactCoverKernel line-inc-exact-kernel)))
              (wholeKernel (Loom.Theory.ExactCoverRect1.toExactCoverKernel line-exact-kernel)))
line-copy-inc-independent env =
  kernel-independence
    (Loom.Theory.ExactCoverRect1.toExactCoverKernel line-exact-kernel)
    (Loom.Theory.ExactCoverRect1.toExactCoverKernel line-inc-exact-kernel)
    (λ ())   -- out-copy (id=6) ≢ out-inc (id=7)
    (λ ())   -- out-inc  (id=7) ≢ in-copy (id=5)
    (λ ())   -- out-copy (id=6) ≢ in-inc  (id=5)
    env

-- ────────────────────────────────────────────────────────────────────
-- Parallel semantics demos
-- ────────────────────────────────────────────────────────────────────

-- parallel-eq-sequential: for line-copy-kernel with forward-schedule,
-- big-step parallel execution produces the same post-state as sequential.
line-copy-parallel-eq-sequential-demo :
  PostStateEq
    (runParallel line-initial line-copy-kernel forward-schedule)
    (runWithSchedule line-initial line-copy-kernel forward-schedule)
line-copy-parallel-eq-sequential-demo arr ix =
  parallel-eq-sequential line-copy-kernel forward-schedule line-initial arr ix

-- parallel-schedule-invariance: forward and backward parallel executions
-- produce the same post-state (formal justification for parFor).
line-copy-parallel-invariance-demo :
  PostStateEq
    (runParallel line-initial line-copy-kernel forward-schedule)
    (runParallel line-initial line-copy-kernel backward-schedule)
line-copy-parallel-invariance-demo =
  parallel-schedule-invariance
    line-copy-kernel
    line-copy-oi-consistent
    forward-schedule
    backward-schedule
    line-initial

-- ────────────────────────────────────────────────────────────────────
-- Confluent reduction demos
-- ────────────────────────────────────────────────────────────────────

-- sumReducer satisfies the comm-assoc condition.
_ : IsCommAssocReducer sumReducer
_ = sumReducer-is-comm-assoc

-- For the identity permutation, foldReducer sum is unchanged (trivial sanity check).
id-perm-3 : PermOn 3
id-perm-3 = record
  { fun        = λ i → i
  ; injective  = λ h → h
  ; surjective = λ k → k , refl
  }

-- foldReducer sumReducer on any permuted 3-element array equals the canonical fold.
sum-perm-demo : (values : Fin 3 → ℕ) →
  foldReducer sumReducer (values ∘ fun id-perm-3) ≡ foldReducer sumReducer values
sum-perm-demo values = foldReducer-sum-perm id-perm-3 values

-- ────────────────────────────────────────────────────────────────────
-- Tagless interpreter-composition demos
-- ────────────────────────────────────────────────────────────────────

-- copy-prog runs under the sequential row-major interpreter
_ : StateM rank2 ⊤
_ = copy-prog (EnvAlg board)

-- copy-prog runs under the column-major interchange interpreter
_ : StateM rank2 ⊤
_ = copy-prog (InterchangeAlg board)

-- The tagless bridge: running copy-prog under EnvAlg equals runRect2
tagless-bridge-demo :
  ∀ (env : Env rank2) →
  proj₁ (copy-prog (EnvAlg board) env) ≡ runRect2 env board-rect-copy
tagless-bridge-demo env = copy-prog-bridge env

-- The main theorem: loop interchange via algebra substitution
-- Same tagless program, two different algebras, same post-state
tagless-interchange-demo :
  ∀ (env : Env rank2) (arr : Array rank2) (ix : RectIx (shape arr)) →
  lookupEnv (proj₁ (copy-prog (EnvAlg board) env)) arr ix ≡
  lookupEnv (proj₁ (copy-prog (InterchangeAlg board) env)) arr ix
tagless-interchange-demo = copy-tagless-interchange
