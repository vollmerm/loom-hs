{-|
Module      : Loom.Verify.Polyhedral
Description : Bridge helpers between verified schedules and polyhedral kernels.
Copyright   :
License     : BSD-3-Clause

The `Loom.Verify.Polyhedral` module provides small adapter types and helper functions for
describing verified schedules and dependences in terms that are easy to lower
into the Loom.Polyhedral API.

Use this module when you want to keep a verified description concise while still
constructing a polyhedral kernel in the public analysis API.

@
phase2DForVerifiedSchedule
  "stencil"
  (TileSchedule2D 32 32)
  VerifiedIndependentDependence2D
  phaseReads
  phaseWrites
  body
@
-}
module Loom.Verify.Polyhedral
  (
    -- * Verified schedule and dependence descriptions
    VerifiedSchedule2D (..)
  , VerifiedDependence2D (..)
    -- * Conversion and construction helpers
  , verifiedSchedule2D
  , verifiedDependence2D
  , phase2DForVerifiedSchedule
    -- * Row-major access helpers
  , rowMajorExpr2D
  , rowMajorRead2D
  , rowMajorWrite2D
  , waveRowMajorRead2D
  , waveRowMajorWrite2D
  ) where

import Loom.Internal.Verify (Prog, WaveOffset (..), waveOffsetDelta)
import qualified Loom.Polyhedral as Poly

-- | Schedule descriptions used by the verified polyhedral bridge helpers.
data VerifiedSchedule2D
  = RectSchedule2D
  | TileSchedule2D !Int !Int
  | WavefrontSchedule2D
  deriving (Eq, Show)

-- | Dependence classifications used by the verified polyhedral helpers.
data VerifiedDependence2D
  = VerifiedIndependentDependence2D
  | VerifiedWavefrontDependence2D
  | VerifiedPrivatizableDependence2D ![String]
  | VerifiedReductionDependence2D ![String]
  | VerifiedOpaqueDependence2D !String
  deriving (Eq, Show)

-- | Convert a verified schedule description into a public polyhedral schedule.
verifiedSchedule2D :: VerifiedSchedule2D -> Poly.Schedule2D
verifiedSchedule2D schedule =
  case schedule of
    RectSchedule2D -> Poly.identitySchedule2D
    TileSchedule2D tileRows tileCols -> Poly.tileSchedule2D tileRows tileCols
    WavefrontSchedule2D -> Poly.wavefrontSchedule2D

-- | Convert a verified dependence description into a public dependence value.
verifiedDependence2D :: VerifiedDependence2D -> Poly.Dependence2D
verifiedDependence2D dependence =
  case dependence of
    VerifiedIndependentDependence2D -> Poly.IndependentDependence2D
    VerifiedWavefrontDependence2D -> Poly.WavefrontDependence2D
    VerifiedPrivatizableDependence2D arrays -> Poly.PrivatizableDependence2D arrays
    VerifiedReductionDependence2D reducers -> Poly.ReductionDependence2D reducers
    VerifiedOpaqueDependence2D message -> Poly.OpaqueDependence2D message

-- | Build a polyhedral phase from verified schedule and dependence metadata.
phase2DForVerifiedSchedule ::
  String ->
  VerifiedSchedule2D ->
  Poly.Dependence2D ->
  [Poly.Access2D] ->
  [Poly.Access2D] ->
  (Int -> Int -> Prog ()) ->
  Poly.Phase2D
phase2DForVerifiedSchedule name schedule dependence phaseReads phaseWrites body =
  Poly.phase2D name (verifiedSchedule2D schedule) dependence phaseReads phaseWrites body

-- | Build a row-major affine expression from row and column expressions.
rowMajorExpr2D :: Int -> Poly.AffineExpr -> Poly.AffineExpr -> Poly.AffineExpr
rowMajorExpr2D stride row col =
  Poly.plus (Poly.scaled stride row) col

-- | Describe a row-major read access.
rowMajorRead2D :: String -> Int -> Poly.AffineExpr -> Poly.AffineExpr -> Poly.Access2D
rowMajorRead2D name stride row col =
  Poly.readAccess2D name (rowMajorExpr2D stride row col)

-- | Describe a row-major write access.
rowMajorWrite2D :: String -> Int -> Poly.AffineExpr -> Poly.AffineExpr -> Poly.Access2D
rowMajorWrite2D name stride row col =
  Poly.writeAccess2D name (rowMajorExpr2D stride row col)

-- | Describe a row-major read access relative to a wavefront offset.
waveRowMajorRead2D :: String -> Int -> (Int, Int) -> WaveOffset -> Poly.Access2D
waveRowMajorRead2D name stride base offset =
  Poly.readAccess2D name (waveRowMajorExpr2D stride base offset)

-- | Describe a row-major write access at the current wavefront position.
waveRowMajorWrite2D :: String -> Int -> (Int, Int) -> Poly.Access2D
waveRowMajorWrite2D name stride base =
  Poly.writeAccess2D name (waveRowMajorExpr2D stride base WaveCurrent)

waveRowMajorExpr2D :: Int -> (Int, Int) -> WaveOffset -> Poly.AffineExpr
waveRowMajorExpr2D stride (baseRow, baseCol) offset =
  rowMajorExpr2D
    stride
    (Poly.plus Poly.rowVar (Poly.constant (baseRow + deltaRow)))
    (Poly.plus Poly.colVar (Poly.constant (baseCol + deltaCol)))
  where
    (deltaRow, deltaCol) = waveOffsetDelta offset
