{-|
Module      : Loom.Verify
Description : Verified loop builders with explicit access metadata.
Copyright   :
License     : BSD-3-Clause

The `Loom.Verify` module exposes a higher-level API for writing kernels together with the
metadata needed for verification-oriented scheduling workflows.

This module is a good fit when you want to:

- describe loop bounds with dedicated verified shapes,
- record reads and writes explicitly,
- use verified tiled or wavefront loop forms,
- and keep the executable kernel and its access information aligned.

For ordinary kernel code, the Loom module is usually the better starting point.
Use Loom.Verify.Polyhedral when you need verified helpers for the polyhedral API.

@
runProg $ parallel $
  parFor2D
    (shape2 rows cols)
    [rectReadAccess2D input 0 0 rows cols]
    [rectWriteAccess2D output 0 0 rows cols]
    $ \\ix -> do
      x <- readAt input ix
      writeAt output ix x
@
-}
module Loom.Verify
  (
    -- * Verified kernel vocabulary
    Rank
  , Schedule
  , Capability
  , WaveOffset (..)
  , Shape
  , Array
  , Index
  , AccessCtx
  , DVec
  , Prog
  , Reducer
  , ReduceVar
    -- * Shapes and extents
  , shape1
  , shape2
  , shape3
  , rectIx1
  , rectIx2
  , rectIx3
  , extent1
  , extent2
  , extent3
  , newArray
  , fromList
  , toList
  , wrapArray
  , unwrapArray
    -- * Proof-carrying bound arrays
  , BoundArr
  , bindArr
  , readBound
  , writeBound
    -- * Proof-carrying shifted-window access
  , ShiftedRange1D
  , proveShiftedRange1D
  , readBoundOffset1D
  , readBoundDVecOffset1D
  , writeBoundDVec1D
    -- * Verified loop forms
  , parFor1D
  , parFor2D
  , parFor3D
  , parForTiled2D
  , parForTiled3D
  , parForWavefront2D
  , foldFor1D
    -- * Access metadata helpers
  , rectReadAccess1D
  , rectReadAccess2D
  , rectWriteAccess1D
  , rectWriteAccess2D
  , rectReadWriteAccess1D
  , rectReadWriteAccess2D
    -- * Reducers and array access
  , newReducer
  , reduce
  , getReducer
  , readAt
  , readOffsetAt1D
  , readDVecAt1D
  , readDVecOffsetAt1D
  , writeAt
  , writeDVecAt1D
  , readWaveAt
  , writeWaveAt
  , rowOf
  , colOf
  , pairOf
  , tileOriginOf
  , tileLocalOf
  , waveCoordsOf
  , unIndex1
  , unIndex2
  , unIndex3
    -- * Running kernels
  , runProg
  , parallel
  , barrier
  , intSum
  , doubleSum
  ) where

import Loom.Internal.Verify
