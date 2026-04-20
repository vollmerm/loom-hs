{-# OPTIONS --safe #-}

-- This module defines the key ingredients for proving that all valid schedules
-- of a kernel are semantically equivalent:
--
--   ValidSchedule    -- a traversal order for a fixed base kernel
--   OutputInputConsistent -- the "no carried dependence" condition
--
-- A ValidSchedule factors ExactCoverKernel into a base kernel (what is computed)
-- and a schedule (how the iteration domain is traversed).  Two valid schedules
-- for the same base kernel can then be compared by ScheduleEquivalence.
--
-- OutputInputConsistent is the Loom analog of the polyhedral model's condition
-- that there are no cross-iteration read-after-write or write-after-write
-- dependences: two steps writing to the same output cell must read from the
-- same input cell.  When this holds, the result is independent of traversal order.

module Loom.Theory.ScheduleIndependent where

open import Loom.Theory.Access
open import Loom.Theory.Examples
  using ( line-copy-kernel
        ; line-inc-kernel
        ; rect-copy-kernel
        ; rect-inc-kernel
        ; tiled-copy-kernel
        ; tiny-rect-copy-kernel
        ; tiny-rect-inc-kernel
        )
open import Loom.Theory.ExactCoverLinear using (ExactCoverKernel)
open import Loom.Theory.Index
open import Loom.Theory.Pointwise
open import Loom.Theory.Prelude
open import Loom.Theory.Schedule
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.Traversal using (LinearTraversal; stepAt)
open import Loom.Theory.WholeLinear using (WholeKernel; runWhole)
import Loom.Theory.ExactCoverLinear as ECL

-- A valid schedule for a base kernel: packages the traversal, injectivity,
-- and exact-cover witness.  This is ExactCoverKernel with the base factored out
-- so that different schedules for the same computation can be compared.
record ValidSchedule
    {rank : Rank} {sched : Schedule rank} {dom : Shape rank}
    (base : PointwiseKernel sched dom)
    (n : ℕ) : Set where
  field
    steps        : LinearTraversal {A = Index sched dom} n
    outputUnique :
      ∀ {i j} →
      resolve (outputAt base (stepAt steps i)) ≡
      resolve (outputAt base (stepAt steps j)) →
      i ≡ j
    coverIndex   : RectIx (shape (outputArr base)) → Fin n
    outputCovered :
      (target : RectIx (shape (outputArr base))) →
      resolve (outputAt base (stepAt steps (coverIndex target))) ≡ target

open ValidSchedule public

-- Build the WholeKernel corresponding to a base + valid schedule.
toWholeKernel :
  ∀ {rank sched dom n} →
  (base : PointwiseKernel {rank} sched dom) →
  ValidSchedule base n →
  WholeKernel {rank} {sched} {dom} n
toWholeKernel base vs = record
  { base         = base
  ; steps        = steps vs
  ; outputUnique = outputUnique vs
  }

-- Bridge to the existing ExactCoverKernel layer.
toExactCoverKernel :
  ∀ {rank sched dom n} →
  (base : PointwiseKernel {rank} sched dom) →
  ValidSchedule base n →
  ExactCoverKernel n
toExactCoverKernel base vs = record
  { wholeKernel  = toWholeKernel base vs
  ; coverIndex   = coverIndex vs
  ; outputCovered = outputCovered vs
  }

-- Run a kernel under a valid schedule.
runWithSchedule :
  ∀ {rank sched dom n} →
  Env rank →
  (base : PointwiseKernel {rank} sched dom) →
  ValidSchedule base n →
  Env rank
runWithSchedule env base vs = runWhole env (toWholeKernel base vs)

-- A kernel is schedule-independent when two steps writing to the same output
-- cell also read from the same input cell.
--
-- This is the formal analog of "no carried cross-iteration dependence" from the
-- polyhedral model.  For pointwise kernels where the input index is an affine
-- function of the output index (e.g. copy, scale, elementwise ops), this holds
-- because the input coordinate is uniquely determined by the output coordinate.
OutputInputConsistent :
  ∀ {rank sched dom} →
  PointwiseKernel {rank} sched dom →
  Set
OutputInputConsistent {rank} {sched} {dom} base =
  ∀ (i j : Index sched dom) →
  resolve (outputAt base i) ≡ resolve (outputAt base j) →
  resolve (inputAt base i)  ≡ resolve (inputAt base j)

-- For the rect-schedule copy and increment kernels, both input and output
-- resolve to the raw rectangular index, so OutputInputConsistent is trivial.

line-copy-oi-consistent : OutputInputConsistent line-copy-kernel
line-copy-oi-consistent _ _ h = h

line-inc-oi-consistent : OutputInputConsistent line-inc-kernel
line-inc-oi-consistent _ _ h = h

rect-copy-oi-consistent : OutputInputConsistent rect-copy-kernel
rect-copy-oi-consistent _ _ h = h

rect-inc-oi-consistent : OutputInputConsistent rect-inc-kernel
rect-inc-oi-consistent _ _ h = h

tiny-rect-copy-oi-consistent : OutputInputConsistent tiny-rect-copy-kernel
tiny-rect-copy-oi-consistent _ _ h = h

tiny-rect-inc-oi-consistent : OutputInputConsistent tiny-rect-inc-kernel
tiny-rect-inc-oi-consistent _ _ h = h

-- For the tiled copy kernel, both input and output resolve to the global
-- coordinate of the tile index, so OutputInputConsistent is equally trivial.
tiled-copy-oi-consistent : OutputInputConsistent tiled-copy-kernel
tiled-copy-oi-consistent _ _ h = h
