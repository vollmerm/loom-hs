{-|
Module      : Loom.Polyhedral
Description : Affine access descriptions, legality analysis, and 2D lowering.
Copyright   :
License     : BSD-3-Clause

The `Loom.Polyhedral` module provides a user-facing API for describing 2D affine loop
phases, analyzing their legality, and lowering legal kernels back to executable
Loom programs.

Typical workflow:

1. describe array accesses and schedules,
2. build a phase with 'phase2D' and a kernel with 'kernel2D',
3. inspect the result with 'summarizeKernel2D' or 'analyzeKernel2D',
4. call 'validateKernel2D' or 'lowerKernel2D' when the kernel is legal.

This module is intended for analysis-oriented code. For the regular kernel DSL,
start in the Loom module instead.

@
phase =
  phase2D
    "copy"
    identitySchedule2D
    IndependentDependence2D
    [readAccess2D "input" (plus (scaled cols rowVar) colVar)]
    [writeAccess2D "output" (plus (scaled cols rowVar) colVar)]
    (\\i j -> ...)

kernel = kernel2D [phase]
@
-}
module Loom.Polyhedral
  (
    -- * Affine expressions and accesses
    AffineVar (..)
  , AffineExpr
  , AccessKind (..)
  , Access2D (..)
  , Dependence2D (..)

    -- * Schedules and summaries
  , Schedule2D
  , PhaseSummary2D (..)
  , KernelSummary2D (..)
  , Legality2D (..)
  , PolyhedralError (..)
  , Phase2D
  , Kernel2D

    -- * Affine expression builders
  , rowVar
  , colVar
  , auxVar
  , constant
  , scaled
  , plus
  , renderAffineExpr
  , readAccess2D
  , writeAccess2D
  , renderAccess2D

    -- * Schedule builders
  , identitySchedule2D
  , affineSchedule2D
  , tileSchedule2D
  , wavefrontSchedule2D
  , composeSchedule2D
  , renderSchedule2D

    -- * Kernel construction and analysis
  , phase2D
  , kernel2D
  , summarizeKernel2D
  , analyzeKernel2D
  , validateKernel2D
  , lowerKernel2D
  ) where

import Loom.Internal.Polyhedral
