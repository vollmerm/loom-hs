{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Loom.Internal.Verify
  ( Rank
  , Schedule
  , Capability
  , WaveOffset (..)
  , waveOffsetDelta
  , Shape
  , Array
  , Index
  , AccessCtx
  , DVec
  , Prog
  , Reducer
  , ReduceVar
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
  , parFor1D
  , parFor2D
  , parFor3D
  , parForTiled2D
  , parForTiled3D
  , parForWavefront2D
  , foldFor1D
  , rectReadAccess1D
  , rectReadAccess2D
  , rectWriteAccess1D
  , rectWriteAccess2D
  , rectReadWriteAccess1D
  , rectReadWriteAccess2D
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
  , runProg
  , parallel
  , barrier
  , intSum
  , doubleSum
  ) where

import Data.Primitive.Types (Prim)
import Loom.Internal.Kernel
  ( Arr
  , DVec
  , Ix1
  , Ix2
  , Ix3
  , Prog
  , Reducer
  , Sh1
  , Sh2
  , Sh3
  , barrier
  , doubleSum
  , foldFor
  , index1
  , index2
  , index3
  , intSum
  , ix1
  , ix2
  , ix3
  , newArr
  , parForTile2D
  , parForTile3D
  , parForSh1
  , parForSh2
  , parForSh3
  , parallel
  , readArr
  , readDVec
  , runProg
  , sh1
  , sh2
  , sh3
  , sizeOfArr
  , tile2D
  , tile3D
  , unIx1
  , unIx2
  , unIx3
  , vecWidth
  , writeArr
  , writeDVec
  )
import qualified Loom.Internal.Kernel as K

-- | Supported verified loop ranks.
data Rank
  = Rank1
  | Rank2
  | Rank3

-- | Schedule families tracked by the verified API.
data Schedule
  = Rect
  | Tile
  | Wavefront

-- | Access capabilities allowed by a verified access context.
data Capability
  = ReadOnly
  | WriteOnly
  | ReadWrite

-- | Relative positions used by wavefront reads and writes.
data WaveOffset
  = WaveCurrent
  | WavePrevRow
  | WavePrevCol
  | WavePrevDiag

waveOffsetDelta :: WaveOffset -> (Int, Int)
{-# INLINE waveOffsetDelta #-}
waveOffsetDelta offset =
  case offset of
    WaveCurrent -> (0, 0)
    WavePrevRow -> (-1, 0)
    WavePrevCol -> (0, -1)
    WavePrevDiag -> (-1, -1)

-- | A verified loop shape with a statically known rank.
data Shape (rank :: Rank) where
  Shape1 :: !Int -> !Sh1 -> Shape 'Rank1
  Shape2 :: !Int -> !Int -> !Sh2 -> Shape 'Rank2
  Shape3 :: !Int -> !Int -> !Int -> !Sh3 -> Shape 'Rank3

-- | An array paired with its verified shape.
data Array (rank :: Rank) a where
  Array :: !(Shape rank) -> !(Arr a) -> Array rank a

-- | An index value tagged by schedule and rank.
data Index (sched :: Schedule) (rank :: Rank) where
  Index1 :: !Ix1 -> Index sched 'Rank1
  Index2 :: !Ix2 -> Index 'Rect 'Rank2
  TileIndex2 :: !Ix2 -> !Ix2 -> Index 'Tile 'Rank2
  WaveIndex2 :: !Ix2 -> !Int -> !Int -> Index 'Wavefront 'Rank2
  Index3 :: !Ix3 -> Index 'Rect 'Rank3
  TileIndex3 :: !Ix3 -> !Ix3 -> Index 'Tile 'Rank3

-- | Access permissions and schedule metadata for verified array operations.
data AccessCtx (cap :: Capability) (sched :: Schedule) (rank :: Rank) where
  Access1 :: !(Shape 'Rank1) -> AccessCtx cap 'Rect 'Rank1
  Access2 :: !(Shape 'Rank2) -> AccessCtx cap 'Rect 'Rank2
  TileAccess2 :: !(Shape 'Rank2) -> AccessCtx cap 'Tile 'Rank2
  WaveAccess2 :: !(Shape 'Rank2) -> AccessCtx cap 'Wavefront 'Rank2
  Access3 :: !(Shape 'Rank3) -> AccessCtx cap 'Rect 'Rank3
  TileAccess3 :: !(Shape 'Rank3) -> AccessCtx cap 'Tile 'Rank3

-- | A verified wrapper around a reducer variable.
newtype ReduceVar a = ReduceVar (K.RedVar a)

class CanRead (cap :: Capability)

instance CanRead 'ReadOnly

instance CanRead 'ReadWrite

class CanWrite (cap :: Capability)

instance CanWrite 'WriteOnly

instance CanWrite 'ReadWrite

-- | Build a rank-1 verified shape.
shape1 :: Int -> Shape 'Rank1
{-# INLINE shape1 #-}
shape1 n = Shape1 n (sh1 n)

-- | Build a rank-2 verified shape.
shape2 :: Int -> Int -> Shape 'Rank2
{-# INLINE shape2 #-}
shape2 rows cols = Shape2 rows cols (sh2 rows cols)

-- | Build a rank-3 verified shape.
shape3 :: Int -> Int -> Int -> Shape 'Rank3
{-# INLINE shape3 #-}
shape3 depth rows cols = Shape3 depth rows cols (sh3 depth rows cols)

-- | Build a rectangular rank-1 index.
rectIx1 :: Int -> Index 'Rect 'Rank1
{-# INLINE rectIx1 #-}
rectIx1 = Index1 . ix1

-- | Build a rectangular rank-2 index.
rectIx2 :: Int -> Int -> Index 'Rect 'Rank2
{-# INLINE rectIx2 #-}
rectIx2 row col = Index2 (ix2 row col)

-- | Build a rectangular rank-3 index.
rectIx3 :: Int -> Int -> Int -> Index 'Rect 'Rank3
{-# INLINE rectIx3 #-}
rectIx3 depth row col = Index3 (ix3 depth row col)

-- | Return the extent of a rank-1 shape.
extent1 :: Shape 'Rank1 -> Int
{-# INLINE extent1 #-}
extent1 (Shape1 n _) = n

-- | Return the extents of a rank-2 shape.
extent2 :: Shape 'Rank2 -> (Int, Int)
{-# INLINE extent2 #-}
extent2 (Shape2 rows cols _) = (rows, cols)

-- | Return the extents of a rank-3 shape.
extent3 :: Shape 'Rank3 -> (Int, Int, Int)
{-# INLINE extent3 #-}
extent3 (Shape3 depth rows cols _) = (depth, rows, cols)

-- | Allocate a verified array with the given shape.
newArray :: Prim a => Shape rank -> IO (Array rank a)
{-# INLINE newArray #-}
newArray shape = Array shape <$> newArr (shapeSize shape)

-- | Allocate and populate a verified array from a list.
fromList :: Prim a => Shape rank -> [a] -> IO (Array rank a)
{-# INLINE fromList #-}
fromList shape xs
  | length xs == shapeSize shape = Array shape <$> K.fromList xs
  | otherwise =
      error
        ( "Loom.Verify.fromList: expected "
            ++ show (shapeSize shape)
            ++ " elements, got "
            ++ show (length xs)
        )

-- | Convert a verified array to a list.
toList :: Prim a => Array rank a -> IO [a]
{-# INLINE toList #-}
toList (Array _ arr) = K.toList arr

-- | Attach a verified shape to an existing array.
wrapArray :: Prim a => Shape rank -> Arr a -> Array rank a
{-# INLINE wrapArray #-}
wrapArray shape arr
  | shapeSize shape == sizeOfArr arr = Array shape arr
  | otherwise =
      error
        ( "Loom.Verify.wrapArray: expected array size "
            ++ show (shapeSize shape)
            ++ ", got "
            ++ show (sizeOfArr arr)
        )

-- | Drop the verified wrapper around an array.
unwrapArray :: Array rank a -> Arr a
{-# INLINE unwrapArray #-}
unwrapArray (Array _ arr) = arr

-- | Run a verified rectangular 1D loop.
parFor1D ::
  Shape 'Rank1 ->
  (AccessCtx 'ReadWrite 'Rect 'Rank1 -> Index 'Rect 'Rank1 -> Prog ()) ->
  Prog ()
{-# INLINE parFor1D #-}
parFor1D shape@(Shape1 _ rawShape) body =
  parForSh1 rawShape (\ix -> body (Access1 shape) (Index1 ix))

-- | Run a verified rectangular 2D loop.
parFor2D ::
  Shape 'Rank2 ->
  (AccessCtx 'ReadWrite 'Rect 'Rank2 -> Index 'Rect 'Rank2 -> Prog ()) ->
  Prog ()
{-# INLINE parFor2D #-}
parFor2D shape@(Shape2 _ _ rawShape) body =
  parForSh2 rawShape (\ix -> body (Access2 shape) (Index2 ix))

-- | Run a verified rectangular 3D loop.
parFor3D ::
  Shape 'Rank3 ->
  (AccessCtx 'ReadWrite 'Rect 'Rank3 -> Index 'Rect 'Rank3 -> Prog ()) ->
  Prog ()
{-# INLINE parFor3D #-}
parFor3D shape@(Shape3 _ _ _ rawShape) body =
  parForSh3 rawShape (\ix -> body (Access3 shape) (Index3 ix))

-- | Run a verified tiled 2D loop.
parForTiled2D ::
  Int ->
  Int ->
  Shape 'Rank2 ->
  (AccessCtx 'ReadWrite 'Tile 'Rank2 -> Index 'Tile 'Rank2 -> Prog ()) ->
  Prog ()
{-# INLINE parForTiled2D #-}
parForTiled2D tileRows tileCols shape@(Shape2 _ _ rawShape) body =
  tile2D tileRows tileCols rawShape $ \row0 col0 ->
    parForTile2D tileRows tileCols row0 col0 rawShape $ \row col ->
      body
        (TileAccess2 shape)
        (TileIndex2 (ix2 row col) (ix2 (row - row0) (col - col0)))

-- | Run a verified tiled 3D loop.
parForTiled3D ::
  Int ->
  Int ->
  Int ->
  Shape 'Rank3 ->
  (AccessCtx 'ReadWrite 'Tile 'Rank3 -> Index 'Tile 'Rank3 -> Prog ()) ->
  Prog ()
{-# INLINE parForTiled3D #-}
parForTiled3D tileDepth tileRows tileCols shape@(Shape3 _ _ _ rawShape) body =
  tile3D tileDepth tileRows tileCols rawShape $ \depth0 row0 col0 ->
    parForTile3D tileDepth tileRows tileCols depth0 row0 col0 rawShape $ \depth row col ->
      body
        (TileAccess3 shape)
        (TileIndex3 (ix3 depth row col) (ix3 (depth - depth0) (row - row0) (col - col0)))

-- | Run a verified 2D wavefront loop.
parForWavefront2D ::
  Shape 'Rank2 ->
  (AccessCtx 'ReadWrite 'Wavefront 'Rank2 -> Index 'Wavefront 'Rank2 -> Prog ()) ->
  Prog ()
{-# INLINE parForWavefront2D #-}
parForWavefront2D shape@(Shape2 _ _ rawShape) body =
  K.parForWavefront2D rawShape $ \rawIx ->
    case unIx2 rawIx of
      (row, col) ->
        body
          (WaveAccess2 shape)
          (WaveIndex2 rawIx (row + col) row)

-- | Reduce the values produced by a verified 1D loop.
foldFor1D :: Reducer a -> Shape 'Rank1 -> (Index 'Rect 'Rank1 -> Prog a) -> Prog a
{-# INLINE foldFor1D #-}
foldFor1D reducer shape body =
  foldFor reducer (shapeSize shape) (\i -> body (Index1 (ix1 i)))

-- | Access context for rectangular rank-1 reads.
rectReadAccess1D :: Shape 'Rank1 -> AccessCtx 'ReadOnly 'Rect 'Rank1
{-# INLINE rectReadAccess1D #-}
rectReadAccess1D = Access1

-- | Access context for rectangular rank-2 reads.
rectReadAccess2D :: Shape 'Rank2 -> AccessCtx 'ReadOnly 'Rect 'Rank2
{-# INLINE rectReadAccess2D #-}
rectReadAccess2D = Access2

-- | Access context for rectangular rank-1 writes.
rectWriteAccess1D :: Shape 'Rank1 -> AccessCtx 'WriteOnly 'Rect 'Rank1
{-# INLINE rectWriteAccess1D #-}
rectWriteAccess1D = Access1

-- | Access context for rectangular rank-2 writes.
rectWriteAccess2D :: Shape 'Rank2 -> AccessCtx 'WriteOnly 'Rect 'Rank2
{-# INLINE rectWriteAccess2D #-}
rectWriteAccess2D = Access2

-- | Access context for rectangular rank-1 reads and writes.
rectReadWriteAccess1D :: Shape 'Rank1 -> AccessCtx 'ReadWrite 'Rect 'Rank1
{-# INLINE rectReadWriteAccess1D #-}
rectReadWriteAccess1D = Access1

-- | Access context for rectangular rank-2 reads and writes.
rectReadWriteAccess2D :: Shape 'Rank2 -> AccessCtx 'ReadWrite 'Rect 'Rank2
{-# INLINE rectReadWriteAccess2D #-}
rectReadWriteAccess2D = Access2

-- | Allocate a verified reducer variable.
newReducer :: Reducer a -> (ReduceVar a -> Prog r) -> Prog r
{-# INLINE newReducer #-}
newReducer reducer body =
  K.newReducer reducer (\redVar -> body (ReduceVar redVar))

-- | Contribute one value to a verified reducer.
reduce :: ReduceVar a -> a -> Prog ()
{-# INLINE reduce #-}
reduce (ReduceVar redVar) x = K.reduce redVar x

-- | Read the final value of a verified reducer.
getReducer :: ReduceVar a -> Prog a
{-# INLINE getReducer #-}
getReducer (ReduceVar redVar) = K.getReducer redVar

-- | Read an element through verified access metadata.
readAt ::
  (Prim a, CanRead cap) =>
  AccessCtx cap sched rank ->
  Array rank a ->
  Index sched rank ->
  Prog a
{-# INLINE readAt #-}
readAt ctx arr ix =
  case (ctx, arr, ix) of
    (ctx1@(Access1 _), Array arrShape rawArr, Index1 rawIx) ->
      checkAccessShape "readAt" ctx1 arrShape `seq`
        readArr rawArr (index1 (rawShape1 arrShape) rawIx)
    (ctx2@(Access2 _), Array arrShape@(Shape2 _ _ _) rawArr, rawIx2) ->
      checkAccessShape "readAt" ctx2 arrShape `seq`
        readArr rawArr (index2 (rawShape2 arrShape) (globalIndex2 rawIx2))
    (ctx2@(TileAccess2 _), Array arrShape@(Shape2 _ _ _) rawArr, rawIx2) ->
      checkAccessShape "readAt" ctx2 arrShape `seq`
        readArr rawArr (index2 (rawShape2 arrShape) (globalIndex2 rawIx2))
    (ctx2@(WaveAccess2 _), Array arrShape@(Shape2 _ _ _) rawArr, rawIx2) ->
      checkAccessShape "readAt" ctx2 arrShape `seq`
        readArr rawArr (index2 (rawShape2 arrShape) (globalIndex2 rawIx2))
    (ctx3@(Access3 _), Array arrShape@(Shape3 _ _ _ _) rawArr, rawIx3) ->
      checkAccessShape "readAt" ctx3 arrShape `seq`
        readArr rawArr (index3 (rawShape3 arrShape) (globalIndex3 rawIx3))
    (ctx3@(TileAccess3 _), Array arrShape@(Shape3 _ _ _ _) rawArr, rawIx3) ->
      checkAccessShape "readAt" ctx3 arrShape `seq`
        readArr rawArr (index3 (rawShape3 arrShape) (globalIndex3 rawIx3))

-- | Read a rank-1 element at a constant offset from the current index.
readOffsetAt1D ::
  (Prim a, CanRead cap) =>
  AccessCtx cap 'Rect 'Rank1 ->
  Array 'Rank1 a ->
  Int ->
  Index 'Rect 'Rank1 ->
  Prog a
{-# INLINE readOffsetAt1D #-}
readOffsetAt1D ctx arr offset ix =
  case (ctx, arr, ix) of
    (ctx1@(Access1 shape), Array arrShape rawArr, Index1 rawIx) ->
      checkAccessShape "readOffsetAt1D" ctx1 arrShape `seq`
        checkShiftedWindow1D "readOffsetAt1D" shape rawIx offset 1 `seq`
          readArr rawArr (unIx1 rawIx + offset)

-- | Read a DVec starting at the current rank-1 index.
readDVecAt1D ::
  CanRead cap =>
  AccessCtx cap 'Rect 'Rank1 ->
  Array 'Rank1 Double ->
  Index 'Rect 'Rank1 ->
  Prog DVec
{-# INLINE readDVecAt1D #-}
readDVecAt1D ctx arr = readDVecOffsetAt1D ctx arr 0

-- | Read a DVec starting at a constant offset from the current rank-1 index.
readDVecOffsetAt1D ::
  CanRead cap =>
  AccessCtx cap 'Rect 'Rank1 ->
  Array 'Rank1 Double ->
  Int ->
  Index 'Rect 'Rank1 ->
  Prog DVec
{-# INLINE readDVecOffsetAt1D #-}
readDVecOffsetAt1D ctx arr offset ix =
  case (ctx, arr, ix) of
    (ctx1@(Access1 shape), Array arrShape rawArr, Index1 rawIx) ->
      checkAccessShape "readDVecOffsetAt1D" ctx1 arrShape `seq`
        checkShiftedWindow1D "readDVecOffsetAt1D" shape rawIx offset vecWidth `seq`
          readDVec rawArr (unIx1 rawIx + offset)

-- | Write an element through verified access metadata.
writeAt ::
  (Prim a, CanWrite cap) =>
  AccessCtx cap sched rank ->
  Array rank a ->
  Index sched rank ->
  a ->
  Prog ()
{-# INLINE writeAt #-}
writeAt ctx arr ix x =
  case (ctx, arr, ix) of
    (ctx1@(Access1 _), Array arrShape rawArr, Index1 rawIx) ->
      checkAccessShape "writeAt" ctx1 arrShape `seq`
        writeArr rawArr (index1 (rawShape1 arrShape) rawIx) x
    (ctx2@(Access2 _), Array arrShape@(Shape2 _ _ _) rawArr, rawIx2) ->
      checkAccessShape "writeAt" ctx2 arrShape `seq`
        writeArr rawArr (index2 (rawShape2 arrShape) (globalIndex2 rawIx2)) x
    (ctx2@(TileAccess2 _), Array arrShape@(Shape2 _ _ _) rawArr, rawIx2) ->
      checkAccessShape "writeAt" ctx2 arrShape `seq`
        writeArr rawArr (index2 (rawShape2 arrShape) (globalIndex2 rawIx2)) x
    (ctx2@(WaveAccess2 _), Array arrShape@(Shape2 _ _ _) rawArr, rawIx2) ->
      checkAccessShape "writeAt" ctx2 arrShape `seq`
        writeArr rawArr (index2 (rawShape2 arrShape) (globalIndex2 rawIx2)) x
    (ctx3@(Access3 _), Array arrShape@(Shape3 _ _ _ _) rawArr, rawIx3) ->
      checkAccessShape "writeAt" ctx3 arrShape `seq`
        writeArr rawArr (index3 (rawShape3 arrShape) (globalIndex3 rawIx3)) x
    (ctx3@(TileAccess3 _), Array arrShape@(Shape3 _ _ _ _) rawArr, rawIx3) ->
      checkAccessShape "writeAt" ctx3 arrShape `seq`
        writeArr rawArr (index3 (rawShape3 arrShape) (globalIndex3 rawIx3)) x

-- | Write a DVec starting at the current rank-1 index.
writeDVecAt1D ::
  CanWrite cap =>
  AccessCtx cap 'Rect 'Rank1 ->
  Array 'Rank1 Double ->
  Index 'Rect 'Rank1 ->
  DVec ->
  Prog ()
{-# INLINE writeDVecAt1D #-}
writeDVecAt1D ctx arr ix vec =
  case (ctx, arr, ix) of
    (ctx1@(Access1 shape), Array arrShape rawArr, Index1 rawIx) ->
      checkAccessShape "writeDVecAt1D" ctx1 arrShape `seq`
        checkShiftedWindow1D "writeDVecAt1D" shape rawIx 0 vecWidth `seq`
          writeDVec rawArr (unIx1 rawIx) vec

-- | Read a wavefront-relative element from a 2D verified array.
readWaveAt ::
  (Prim a, CanRead cap) =>
  AccessCtx cap 'Wavefront 'Rank2 ->
  Array 'Rank2 a ->
  (Int, Int) ->
  WaveOffset ->
  Index 'Wavefront 'Rank2 ->
  Prog a
{-# INLINE readWaveAt #-}
readWaveAt ctx arr base offset ix =
  case (ctx, arr) of
    (WaveAccess2 waveShape, Array arrShape rawArr) ->
      checkWaveAccessShape "readWaveAt" waveShape arrShape base offset `seq`
      let targetIx = waveArrayIndex arrShape base offset ix
       in readArr rawArr (index2 (rawShape2 arrShape) targetIx)

-- | Write the current wavefront position in a 2D verified array.
writeWaveAt ::
  (Prim a, CanWrite cap) =>
  AccessCtx cap 'Wavefront 'Rank2 ->
  Array 'Rank2 a ->
  (Int, Int) ->
  Index 'Wavefront 'Rank2 ->
  a ->
  Prog ()
{-# INLINE writeWaveAt #-}
writeWaveAt ctx arr base ix x =
  case (ctx, arr) of
    (WaveAccess2 waveShape, Array arrShape rawArr) ->
      checkWaveAccessShape "writeWaveAt" waveShape arrShape base WaveCurrent `seq`
      let targetIx = waveArrayIndex arrShape base WaveCurrent ix
       in writeArr rawArr (index2 (rawShape2 arrShape) targetIx) x

-- | Project the row component of a rank-2 index.
rowOf :: Index sched 'Rank2 -> Index 'Rect 'Rank1
{-# INLINE rowOf #-}
rowOf ix =
  case unIndex2 ix of
    (row, _) -> Index1 (ix1 row)

-- | Project the column component of a rank-2 index.
colOf :: Index sched 'Rank2 -> Index 'Rect 'Rank1
{-# INLINE colOf #-}
colOf ix =
  case unIndex2 ix of
    (_, col) -> Index1 (ix1 col)

-- | Combine two rank-1 indices into one rectangular rank-2 index.
pairOf :: Index 'Rect 'Rank1 -> Index 'Rect 'Rank1 -> Index 'Rect 'Rank2
{-# INLINE pairOf #-}
pairOf (Index1 row) (Index1 col) = Index2 (ix2 (unIx1 row) (unIx1 col))

-- | Extract the coordinate stored in a rank-1 index.
unIndex1 :: Index sched 'Rank1 -> Int
{-# INLINE unIndex1 #-}
unIndex1 (Index1 rawIx) = unIx1 rawIx

-- | Extract the global coordinates stored in a rank-2 index.
unIndex2 :: Index sched 'Rank2 -> (Int, Int)
{-# INLINE unIndex2 #-}
unIndex2 (Index2 rawIx) = unIx2 rawIx
unIndex2 (TileIndex2 rawIx _) = unIx2 rawIx
unIndex2 (WaveIndex2 rawIx _ _) = unIx2 rawIx

-- | Extract the global coordinates stored in a rank-3 index.
unIndex3 :: Index sched 'Rank3 -> (Int, Int, Int)
{-# INLINE unIndex3 #-}
unIndex3 (Index3 rawIx) = unIx3 rawIx
unIndex3 (TileIndex3 rawIx _) = unIx3 rawIx

-- | Return the origin of the current 2D tile.
tileOriginOf :: Index 'Tile 'Rank2 -> (Int, Int)
{-# INLINE tileOriginOf #-}
tileOriginOf (TileIndex2 rawIx localIx) =
  let (row, col) = unIx2 rawIx
      (localRow, localCol) = unIx2 localIx
   in (row - localRow, col - localCol)

-- | Return the local coordinates within the current 2D tile.
tileLocalOf :: Index 'Tile 'Rank2 -> (Int, Int)
{-# INLINE tileLocalOf #-}
tileLocalOf (TileIndex2 _ localIx) = unIx2 localIx

-- | Return the wavefront diagonal and position-along-diagonal.
waveCoordsOf :: Index 'Wavefront 'Rank2 -> (Int, Int)
{-# INLINE waveCoordsOf #-}
waveCoordsOf (WaveIndex2 _ diag along) = (diag, along)

shapeSize :: Shape rank -> Int
{-# INLINE shapeSize #-}
shapeSize (Shape1 n _) = n
shapeSize (Shape2 rows cols _) = rows * cols
shapeSize (Shape3 depth rows cols _) = depth * rows * cols

rawShape1 :: Shape 'Rank1 -> Sh1
{-# INLINE rawShape1 #-}
rawShape1 (Shape1 _ rawShape) = rawShape

rawShape2 :: Shape 'Rank2 -> Sh2
{-# INLINE rawShape2 #-}
rawShape2 (Shape2 _ _ rawShape) = rawShape

rawShape3 :: Shape 'Rank3 -> Sh3
{-# INLINE rawShape3 #-}
rawShape3 (Shape3 _ _ _ rawShape) = rawShape

sameShape :: Shape rank -> Shape rank -> Bool
{-# INLINE sameShape #-}
sameShape (Shape1 left _) (Shape1 right _) = left == right
sameShape (Shape2 leftRows leftCols _) (Shape2 rightRows rightCols _) =
  leftRows == rightRows && leftCols == rightCols
sameShape (Shape3 leftDepth leftRows leftCols _) (Shape3 rightDepth rightRows rightCols _) =
  leftDepth == rightDepth && leftRows == rightRows && leftCols == rightCols

checkMatchingShape :: String -> Shape rank -> Shape rank -> ()
{-# INLINE checkMatchingShape #-}
checkMatchingShape label expected actual
  | sameShape expected actual = ()
  | otherwise =
      error
        ( "Loom.Verify."
            ++ label
            ++ ": loop shape "
            ++ renderShape expected
            ++ " does not match array shape "
            ++ renderShape actual
        )

renderShape :: Shape rank -> String
{-# INLINE renderShape #-}
renderShape (Shape1 n _) = "sh1(" ++ show n ++ ")"
renderShape (Shape2 rows cols _) = "sh2(" ++ show rows ++ "," ++ show cols ++ ")"
renderShape (Shape3 depth rows cols _) = "sh3(" ++ show depth ++ "," ++ show rows ++ "," ++ show cols ++ ")"

checkAccessShape :: String -> AccessCtx cap sched rank -> Shape rank -> ()
{-# INLINE checkAccessShape #-}
checkAccessShape label ctx =
  case ctx of
    Access1 shape -> checkMatchingShape label shape
    Access2 shape -> checkMatchingShape label shape
    TileAccess2 shape -> checkMatchingShape label shape
    WaveAccess2 shape -> checkMatchingShape label shape
    Access3 shape -> checkMatchingShape label shape
    TileAccess3 shape -> checkMatchingShape label shape

checkShiftedWindow1D :: String -> Shape 'Rank1 -> Ix1 -> Int -> Int -> ()
{-# INLINE checkShiftedWindow1D #-}
checkShiftedWindow1D label (Shape1 n _) rawIx offset width
  | base < 0 =
      error
        ( "Loom.Verify."
            ++ label
            ++ ": shifted access starts before the array: base="
            ++ show base
            ++ ", width="
            ++ show width
            ++ ", extent="
            ++ show n
        )
  | base + width > n =
      error
        ( "Loom.Verify."
            ++ label
            ++ ": shifted access exceeds the array: base="
            ++ show base
            ++ ", width="
            ++ show width
            ++ ", extent="
            ++ show n
        )
  | otherwise = ()
  where
    base = unIx1 rawIx + offset

checkWaveAccessShape :: String -> Shape 'Rank2 -> Shape 'Rank2 -> (Int, Int) -> WaveOffset -> ()
{-# INLINE checkWaveAccessShape #-}
checkWaveAccessShape label (Shape2 waveRows waveCols _) arrShape@(Shape2 arrRows arrCols _) (baseRow, baseCol) offset
  | baseRow + deltaRowMin < 0 =
      error (outOfBoundsMessage "row base is negative for offset")
  | baseCol + deltaColMin < 0 =
      error (outOfBoundsMessage "column base is negative for offset")
  | baseRow + waveRows + deltaRowMax > arrRows =
      error (outOfBoundsMessage "row extent exceeds target array")
  | baseCol + waveCols + deltaColMax > arrCols =
      error (outOfBoundsMessage "column extent exceeds target array")
  | otherwise =
      ()
  where
    (deltaRowMin, deltaRowMax, deltaColMin, deltaColMax) =
      case offset of
        WaveCurrent -> (0, 0, 0, 0)
        WavePrevRow -> (-1, -1, 0, 0)
        WavePrevCol -> (0, 0, -1, -1)
        WavePrevDiag -> (-1, -1, -1, -1)

    outOfBoundsMessage detail =
      "Loom.Verify."
        ++ label
        ++ ": wave domain "
        ++ renderShape (Shape2 waveRows waveCols (sh2 waveRows waveCols))
        ++ " with base "
        ++ show (baseRow, baseCol)
        ++ " and offset "
        ++ showWaveOffset offset
        ++ " does not fit target array "
        ++ renderShape arrShape
        ++ " ("
        ++ detail
        ++ ")"

showWaveOffset :: WaveOffset -> String
{-# INLINE showWaveOffset #-}
showWaveOffset WaveCurrent = "current"
showWaveOffset WavePrevRow = "prev-row"
showWaveOffset WavePrevCol = "prev-col"
showWaveOffset WavePrevDiag = "prev-diag"

globalIndex2 :: Index sched 'Rank2 -> Ix2
{-# INLINE globalIndex2 #-}
globalIndex2 (Index2 rawIx) = rawIx
globalIndex2 (TileIndex2 rawIx _) = rawIx
globalIndex2 (WaveIndex2 rawIx _ _) = rawIx

globalIndex3 :: Index sched 'Rank3 -> Ix3
{-# INLINE globalIndex3 #-}
globalIndex3 (Index3 rawIx) = rawIx
globalIndex3 (TileIndex3 rawIx _) = rawIx

waveArrayIndex :: Shape 'Rank2 -> (Int, Int) -> WaveOffset -> Index 'Wavefront 'Rank2 -> Ix2
{-# INLINE waveArrayIndex #-}
waveArrayIndex arrShape (baseRow, baseCol) offset ix =
  let (row, col) = unIndex2 ix
      (deltaRow, deltaCol) = waveOffsetDelta offset
      targetRow = row + baseRow + deltaRow
      targetCol = col + baseCol + deltaCol
   in checkWaveBounds arrShape targetRow targetCol

checkWaveBounds :: Shape 'Rank2 -> Int -> Int -> Ix2
{-# INLINE checkWaveBounds #-}
checkWaveBounds shape@(Shape2 rows cols _ ) row col
  | rows <= 0 || cols <= 0 =
      error "Loom.Verify.wave access requires a non-empty 2D array"
  | row >= 0 && row < rows && col >= 0 && col < cols =
      ix2 row col
  | otherwise =
      error
        ( "Loom.Verify.wave access out of bounds: ("
            ++ show row
            ++ ","
            ++ show col
            ++ ") for "
            ++ renderShape shape
        )
