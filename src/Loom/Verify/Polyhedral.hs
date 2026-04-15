module Loom.Verify.Polyhedral
  ( VerifiedSchedule2D (..)
  , VerifiedDependence2D (..)
  , verifiedSchedule2D
  , verifiedDependence2D
  , phase2DForVerifiedSchedule
  , rowMajorExpr2D
  , rowMajorRead2D
  , rowMajorWrite2D
  , waveRowMajorRead2D
  , waveRowMajorWrite2D
  ) where

import Loom.Internal.Verify (Prog, WaveOffset (..), waveOffsetDelta)
import qualified Loom.Polyhedral as Poly

data VerifiedSchedule2D
  = RectSchedule2D
  | TileSchedule2D !Int !Int
  | WavefrontSchedule2D
  deriving (Eq, Show)

data VerifiedDependence2D
  = VerifiedIndependentDependence2D
  | VerifiedWavefrontDependence2D
  | VerifiedPrivatizableDependence2D ![String]
  | VerifiedReductionDependence2D ![String]
  | VerifiedOpaqueDependence2D !String
  deriving (Eq, Show)

verifiedSchedule2D :: VerifiedSchedule2D -> Poly.Schedule2D
verifiedSchedule2D schedule =
  case schedule of
    RectSchedule2D -> Poly.identitySchedule2D
    TileSchedule2D tileRows tileCols -> Poly.tileSchedule2D tileRows tileCols
    WavefrontSchedule2D -> Poly.wavefrontSchedule2D

verifiedDependence2D :: VerifiedDependence2D -> Poly.Dependence2D
verifiedDependence2D dependence =
  case dependence of
    VerifiedIndependentDependence2D -> Poly.IndependentDependence2D
    VerifiedWavefrontDependence2D -> Poly.WavefrontDependence2D
    VerifiedPrivatizableDependence2D arrays -> Poly.PrivatizableDependence2D arrays
    VerifiedReductionDependence2D reducers -> Poly.ReductionDependence2D reducers
    VerifiedOpaqueDependence2D message -> Poly.OpaqueDependence2D message

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

rowMajorExpr2D :: Int -> Poly.AffineExpr -> Poly.AffineExpr -> Poly.AffineExpr
rowMajorExpr2D stride row col =
  Poly.plus (Poly.scaled stride row) col

rowMajorRead2D :: String -> Int -> Poly.AffineExpr -> Poly.AffineExpr -> Poly.Access2D
rowMajorRead2D name stride row col =
  Poly.readAccess2D name (rowMajorExpr2D stride row col)

rowMajorWrite2D :: String -> Int -> Poly.AffineExpr -> Poly.AffineExpr -> Poly.Access2D
rowMajorWrite2D name stride row col =
  Poly.writeAccess2D name (rowMajorExpr2D stride row col)

waveRowMajorRead2D :: String -> Int -> (Int, Int) -> WaveOffset -> Poly.Access2D
waveRowMajorRead2D name stride base offset =
  Poly.readAccess2D name (waveRowMajorExpr2D stride base offset)

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
