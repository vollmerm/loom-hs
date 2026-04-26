{-|
Module      : Loom.Expert
Description : Low-level shapes, schedules, and loop primitives for expert use.
Copyright   :
License     : BSD-3-Clause

The `Loom.Expert` module exposes the low-level building blocks that sit underneath the
shape-first Loom front door.

Import this module when you need direct control over:

- raw indices and shapes,
- explicit affine or scheduled loop lowering,
- specialized tiled or transformed loop forms,
- or code that needs the representation-oriented API directly.

Most users should start in the Loom and Loom.Schedule modules. Reach for this module
when you want to describe the exact execution model directly.

@
import Loom.Expert

runProg $ parallel $
  tiledFor2D 32 32 (sh2 rows cols) $ \\i j -> ...
@
-}
module Loom.Expert
  (
    -- * Core types
    Arr
  , Ix1
  , Ix2
  , Ix3
  , IxN
  , Sh1
  , Sh2
  , Sh3
  , ShN
  , Rect2
  , RectN
  , AffineN
  , Affine2
  , ScheduleN
  , Transform2D
  , Vec
  , IVec
  , I32Vec
  , DVec
  , Prog
  , Reducer
  , RedVar
  , AccVar
    -- * Shape and index construction
  , ix1
  , ix2
  , ix3
  , ixN
  , sh1
  , sh2
  , sh3
  , shN
  , rect2
  , rectN
  , affineN
  , affine2
  , newArr
  , fromList
  , toList
  , unIx1
  , unIx2
  , unIx3
  , unIxN
  , unSh2
  , unShN
  , unRect2
  , unRectN
  , withIx2
  , withIx3
  , index1
  , index2
  , index3
  , indexN
    -- * Affine maps and schedules
  , applyAffineN
  , composeAffineN
  , invertAffineN
  , identityAffineN
  , permuteAffineN
  , boundingBoxAffineN
  , applyAffine2
  , composeAffine2
  , invertAffine2
  , identityAffine2
  , interchange2D
  , skew2D
  , boundingBoxAffine2D
  , identityScheduleN
  , affineScheduleN
  , tileScheduleN
  , permuteScheduleN
  , composeScheduleN
  , renderScheduleN
  , identityTransform2D
  , affineTransform2D
  , tileTransform2D
  , composeTransform2D
  , interchangeTransform2D
  , skewTransform2D
    -- * Arrays, execution, and loop forms
  , vecWidth
  , sizeOfArr
  , readArrIO
  , writeArrIO
  , runProg
  , parallel
  , barrier
  , parFor
  , parForSh1
  , parForSh2
  , parForSh3
  , parForShN
  , parFor2
  , parFor3
  , parForRect2D
  , parForRows
  , parForRowsRect2D
  , parForCheckerboard
  , parForRectN
  , parForScheduleN
  , parForAffineRect2D
  , parForAffine2D
  , parForWavefront2D
  , tileRect2D
  , tileN
  , tiledForRect2D
  , parForTransform2D
  , tile2D
  , tile3D
  , parForTileN
  , parForTile2D
  , parForTile3D
  , tiledForN
  , tiledFor2D
  , tiledFor3D
  , stripMine
    -- * SIMD operations
  , broadcastVec
  , broadcastIVec
  , broadcastI32Vec
  , broadcastDVec
  , readArr
  , writeArr
  , readVec
  , readIVec
  , readI32Vec
  , readDVec
  , writeVec
  , writeIVec
  , writeI32Vec
  , writeDVec
  , addVec
  , addIVec
  , addI32Vec
  , addDVec
  , mulVec
  , mulIVec
  , mulI32Vec
  , mulDVec
  , sumVec
  , sumIVec
  , sumI32Vec
  , sumDVec
    -- * Reductions and accumulators
  , newReducer
  , reduce
  , getReducer
  , accumFor
  , accumVecFor
  , accumIVecFor
  , accumI32VecFor
  , accumDVecFor
  , newAcc
  , readAcc
  , writeAcc
  , foldFor
  , mkReducer
  , mkReducerWith
  , intSum
  , int32Sum
  , doubleSum
  ) where

import Loom.Internal.Kernel
