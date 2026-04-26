{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}

#include "MachDeps.h"

module Loom.Internal.Kernel
  ( Arr
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
  , parForSlices
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
  , subDVec
  , sumVec
  , sumIVec
  , sumI32Vec
  , sumDVec
  , invSqrtDVec
  , newReducer
  , reduce
  , getReducer
  , accumFor
  , accumVecFor
  , accumIVecFor
  , accumI32VecFor
  , accumDVecFor
  , foldSimdFor4
  , newAcc
  , readAcc
  , writeAcc
  , foldFor
  , parFoldFor
  , mkReducer
  , mkReducerWith
  , intSum
  , int32Sum
  , doubleSum
  ) where

import Control.Concurrent (MVar, forkIOWithUnmask, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.MVar (modifyMVar, newMVar)
import Control.Exception (SomeException, bracket, throwIO, try)
import Control.Monad (ap)
import Control.Monad.Primitive (primitive)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (intercalate, sort)
import Data.Primitive.Array
  ( MutableArray
  , newArray
  , readArray
  , writeArray
  )
import Data.Primitive.PrimArray
  ( MutablePrimArray (..)
  , newPrimArray
  , readPrimArray
  , writePrimArray
  )
import Data.Primitive.PrimVar
  ( PrimVar
  , newPrimVar
  , readPrimVar
  , writePrimVar
  )
import Data.Primitive.Types (Prim)
import Data.Ratio ((%), denominator, numerator)
import GHC.Conc (getNumCapabilities)
import GHC.Exts hiding
  ( fromList
  , toList
  )
import GHC.Int (Int32 (..))
import System.IO.Unsafe (unsafePerformIO)

-- | A mutable flat primitive array.
--
-- Arrays are the main storage abstraction used by Loom kernels.
data Arr a = Arr !Int !(MutablePrimArray RealWorld a)

-- | A one-dimensional index.
newtype Ix1 = Ix1 Int

-- | A two-dimensional index.
data Ix2 = Ix2 {-# UNPACK #-} !Int {-# UNPACK #-} !Int

-- | A three-dimensional index.
data Ix3 = Ix3 {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int

-- | An index of arbitrary rank.
newtype IxN = IxN [Int]

-- | A one-dimensional shape.
newtype Sh1 = Sh1 Int

-- | A two-dimensional shape.
data Sh2 = Sh2 {-# UNPACK #-} !Int {-# UNPACK #-} !Int

-- | A three-dimensional shape.
data Sh3 = Sh3 {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int

-- | A shape of arbitrary rank.
newtype ShN = ShN [Int]

data Rect2 =
  Rect2
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int

data RectN = RectN ![Int] ![Int]

data AffineN = AffineN ![[Int]] ![Int]

data Affine2 =
  Affine2
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int

data Domain2D
  = RectDomain !Rect2
  | SkewDomain !Rect2 !Int

data ScheduleStageN
  = ScheduleAffineStageN !AffineN
  | ScheduleTileStageN ![Int]

-- | A rank-polymorphic loop schedule.
newtype ScheduleN = ScheduleN [ScheduleStageN]

-- | A compiled 2D transform representation used by specialized lowerings.
newtype Transform2D = Transform2D TransformExpr2D

data TransformExpr2D
  = TransformIdentity2D
  | TransformAffineStage2D !Affine2
  | TransformTileStage2D !Int !Int
  | TransformCompose2D !TransformExpr2D !TransformExpr2D

data RawTransformStage2D
  = RawAffineStage2D !Affine2
  | RawTileStage2D !Int !Int

data CompiledAffine2D
  = CompiledIdentity2D
  | CompiledInterchange2D
  | CompiledSkew2D !Int
  | CompiledGenericAffine2D !Affine2

data NormalizedTransformStage2D
  = NormalizedAffineStage2D !CompiledAffine2D
  | NormalizedTileStage2D !Int !Int

newtype NormalizedTransform2D = NormalizedTransform2D [NormalizedTransformStage2D]

-- | A width-4 logical vector value.
--
-- Specialized constructors are used for element types with dedicated SIMD
-- representations.
data Vec a where
  Vec :: !a -> !a -> !a -> !a -> Vec a
#if WORD_SIZE_IN_BITS == 64
  VecInt :: Int64X2# -> Int64X2# -> Vec Int
#else
  VecInt :: Int32X4# -> Vec Int
#endif
  VecInt32 :: Int32X4# -> Vec Int32
  VecDouble :: DoubleX2# -> DoubleX2# -> Vec Double

#if WORD_SIZE_IN_BITS == 64
-- | A SIMD-sized vector of machine integers.
data IVec = IVec Int64X2# Int64X2#
#else
data IVec = IVec Int32X4#
#endif

-- | A SIMD-sized vector of 'Int32' values.
data I32Vec = I32Vec Int32X4#

-- | A SIMD-sized vector of 'Double' values.
data DVec = DVec DoubleX2# DoubleX2#

class (Prim a, Num a) => VecOps a where
  readVecIO :: MutablePrimArray RealWorld a -> Int -> IO (Vec a)
  writeVecIO :: MutablePrimArray RealWorld a -> Int -> Vec a -> IO ()
  broadcastVecIO :: a -> Vec a
  addVecIO :: Vec a -> Vec a -> Vec a
  mulVecIO :: Vec a -> Vec a -> Vec a
  sumVecIO :: Vec a -> a

instance {-# OVERLAPPABLE #-} (Prim a, Num a) => VecOps a where
  readVecIO = readVecScalar
  writeVecIO = writeVecScalar
  broadcastVecIO = broadcastVecScalar
  addVecIO = addVecScalar
  mulVecIO = mulVecScalar
  sumVecIO = sumVecScalar

instance VecOps Int where
  readVecIO = readVecInt
  writeVecIO = writeVecInt
  broadcastVecIO = broadcastVecInt
  addVecIO = addVecInt
  mulVecIO = mulVecInt
  sumVecIO = sumVecInt

instance VecOps Int32 where
  readVecIO = readVecInt32
  writeVecIO = writeVecInt32
  broadcastVecIO = broadcastVecInt32
  addVecIO = addVecInt32
  mulVecIO = mulVecInt32
  sumVecIO = sumVecInt32

instance VecOps Double where
  readVecIO = readVecDouble
  writeVecIO = writeVecDouble
  broadcastVecIO = broadcastVecDouble
  addVecIO = addVecDouble
  mulVecIO = mulVecDouble
  sumVecIO = sumVecDouble

data ReducerSpec rep a = ReducerSpec
  { reducerInit :: !rep
  , reducerStep :: rep -> a -> rep
  , reducerMerge :: rep -> rep -> rep
  , reducerDone :: rep -> a
  }

-- | A parallel reduction strategy.
data Reducer a where
  Reducer :: Prim rep => !(ReducerSpec rep a) -> Reducer a

-- | A handle to reducer state allocated inside a kernel.
data RedVar a where
  LocalRedVar ::
    Prim rep =>
    !(PrimVar RealWorld rep) ->
    !(ReducerSpec rep a) ->
    RedVar a
  SharedRedVar ::
    Prim rep =>
    !(MutableArray RealWorld (PrimVar RealWorld rep)) ->
    !Int ->
    !(ReducerSpec rep a) ->
    RedVar a

-- | A mutable scalar accumulator allocated inside a kernel.
data AccVar a where
  AccVar :: Prim a => !(PrimVar RealWorld a) -> AccVar a

data WorkerHandle = WorkerHandle
  { workerCommand :: !(MVar WorkerCommand)
  , workerResult :: !(MVar (Either SomeException ()))
  }

data WorkerCommand
  = RunPhase !Phase
  | RunStaticWork !Int !Int !(Int -> Int -> Int -> IO ())
  | StopWorker

data Phase = Phase
  { phaseEnd :: !Int
  , phaseChunkSize :: !Int
  , phaseNextIndex :: !(IORef Int)
  , phaseFailure :: !(IORef (Maybe SomeException))
  , phaseRunChunk :: !(Int -> Int -> Int -> IO ())
  }

data Team = Team
  { teamWorkerCount :: !Int
  , teamWorkers :: ![WorkerHandle]
  }

class Monad repr => Loop repr where
  loopParallel :: repr a -> repr a
  loopBarrier :: repr ()
  loopParFor :: Int -> (Int -> repr ()) -> repr ()
  loopParFor2# :: Int# -> Int# -> (Int# -> Int# -> repr ()) -> repr ()
  loopParFor3# :: Int# -> Int# -> Int# -> (Int# -> Int# -> Int# -> repr ()) -> repr ()
  loopReadArr :: Prim a => Arr a -> Int -> repr a
  loopWriteArr :: Prim a => Arr a -> Int -> a -> repr ()
  loopReadVec :: VecOps a => Arr a -> Int -> repr (Vec a)
  loopWriteVec :: VecOps a => Arr a -> Int -> Vec a -> repr ()
  loopReadIVec :: Arr Int -> Int -> repr IVec
  loopWriteIVec :: Arr Int -> Int -> IVec -> repr ()
  loopReadI32Vec :: Arr Int32 -> Int -> repr I32Vec
  loopWriteI32Vec :: Arr Int32 -> Int -> I32Vec -> repr ()
  loopReadDVec :: Arr Double -> Int -> repr DVec
  loopWriteDVec :: Arr Double -> Int -> DVec -> repr ()
  loopNewReducer :: Reducer a -> (RedVar a -> repr r) -> repr r
  loopReduce :: RedVar a -> a -> repr ()
  loopGetReducer :: RedVar a -> repr a
  loopAccumFor :: Int -> a -> (a -> Int -> repr a) -> repr a
  loopAccumVecFor :: VecAccum a => Int -> Vec a -> (Vec a -> Int -> repr (Vec a)) -> repr (Vec a)
  loopAccumIVecFor :: Int -> IVec -> (IVec -> Int -> repr IVec) -> repr IVec
  loopAccumI32VecFor :: Int -> I32Vec -> (I32Vec -> Int -> repr I32Vec) -> repr I32Vec
  loopAccumDVecFor :: Int -> DVec -> (DVec -> Int -> repr DVec) -> repr DVec
  loopNewAcc :: Prim a => a -> (AccVar a -> repr r) -> repr r
  loopReadAcc :: Prim a => AccVar a -> repr a
  loopWriteAcc :: Prim a => AccVar a -> a -> repr ()
  loopFoldFor :: Reducer a -> Int -> (Int -> repr a) -> repr a
  loopParFoldFor :: Reducer a -> Int -> (Int -> repr a) -> repr a

-- | The program type used by Loom kernels.
--
-- 'Prog' is a monad for array reads, writes, loop construction, and reduction
-- operations that ultimately runs in 'IO' via 'runProg'.
newtype Prog a = Prog
  { unProg :: forall repr r. Loop repr => (a -> repr r) -> repr r
  }

instance Functor Prog where
  fmap f (Prog m) = Prog $ \k -> m (k . f)

instance Applicative Prog where
  pure x = Prog ($ x)
  (<*>) = ap

instance Monad Prog where
  Prog m >>= f = Prog $ \k -> m (\a -> unProg (f a) k)

data Runtime = Runtime
  { rtTeam :: !(Maybe Team)
  , rtWorkerId :: !(Maybe Int)
  , rtLoopDepth :: !Int
  }

newtype Kernel a = Kernel
  { runKernel :: Runtime -> IO a
  }

instance Functor Kernel where
  fmap f (Kernel action) = Kernel (\rt -> fmap f (action rt))

instance Applicative Kernel where
  pure x = Kernel (\_ -> pure x)
  Kernel mf <*> Kernel mx = Kernel (\rt -> mf rt <*> mx rt)

instance Monad Kernel where
  Kernel action >>= f = Kernel $ \rt -> do
    x <- action rt
    runKernel (f x) rt

instance Loop Kernel where
  loopParallel body = Kernel $ \rt ->
    case (rtTeam rt, rtWorkerId rt, rtLoopDepth rt) of
      (Nothing, Nothing, 0) -> do
        caps <- getNumCapabilities
        let !workers = max 1 caps
        withGlobalTeam workers $ \team ->
          runKernel body (Runtime (Just team) Nothing 0)
      _ ->
        invalidUsage "parallel regions cannot be nested or entered from inside a loop body"

  loopBarrier = Kernel $ \rt ->
    case (rtTeam rt, rtWorkerId rt, rtLoopDepth rt) of
      (Nothing, _, _) ->
        invalidUsage "barrier may only be used inside a parallel region"
      (_, Just _, _) ->
        invalidUsage "barrier may not appear inside a parallel loop body"
      (_, _, depth)
        | depth > 0 ->
            invalidUsage "barrier may only appear between parallel phases"
      _ ->
        pure ()

  {-# INLINE loopParFor #-}
  loopParFor n body = Kernel $ \rt ->
    dispatchLoop rt n $ \workerRt start end ->
      let go !i
            | i >= end = pure ()
            | otherwise = runKernel (body i) workerRt >> go (i + 1)
       in go start

  {-# INLINE loopParFor2# #-}
  loopParFor2# n# m# body = Kernel $ \rt ->
    case n# <=# 0# of
      1# -> pure ()
      _ ->
        case m# <=# 0# of
          1# -> pure ()
          _ ->
            let !total = I# (n# *# m#)
             in dispatchLoop rt total $ \workerRt start end ->
                  runLinear2D workerRt m# body start end

  {-# INLINE loopParFor3# #-}
  loopParFor3# n# m# p# body = Kernel $ \rt ->
    case n# <=# 0# of
      1# -> pure ()
      _ ->
        case m# <=# 0# of
          1# -> pure ()
          _ ->
            case p# <=# 0# of
              1# -> pure ()
              _ ->
                let !plane# = m# *# p#
                    !total = I# (n# *# plane#)
                 in dispatchLoop rt total $ \workerRt start end ->
                      runLinear3D workerRt m# p# body start end

  loopReadArr (Arr _ arr) i = Kernel (\_ -> readPrimArray arr i)

  loopWriteArr (Arr _ arr) i x = Kernel (\_ -> writePrimArray arr i x)

  loopReadVec (Arr _ arr) i = Kernel (\_ -> readVecIO arr i)

  loopWriteVec (Arr _ arr) i vec = Kernel (\_ -> writeVecIO arr i vec)

  loopReadIVec (Arr _ arr) i = Kernel (\_ -> readIVecIO arr i)

  loopWriteIVec (Arr _ arr) i vec = Kernel (\_ -> writeIVecIO arr i vec)

  loopReadI32Vec (Arr _ arr) i = Kernel (\_ -> readI32VecIO arr i)

  loopWriteI32Vec (Arr _ arr) i vec = Kernel (\_ -> writeI32VecIO arr i vec)

  loopReadDVec (Arr _ arr) i = Kernel (\_ -> readDVecIO arr i)

  loopWriteDVec (Arr _ arr) i vec = Kernel (\_ -> writeDVecIO arr i vec)

  loopNewReducer reducer body =
    Kernel $ \rt -> do
      redVar <-
        if shouldShareReducer rt
          then newSharedRedVar (parallelWorkers rt) reducer
          else newLocalRedVar reducer
      runKernel (body redVar) rt

  loopReduce redVar x =
    Kernel $ \rt ->
      case redVar of
        LocalRedVar var spec -> do
          !acc <- readPrimVar var
          let !acc' = reducerStep spec acc x
          writePrimVar var acc'
        SharedRedVar vars _ spec -> do
          let !slot = reducerSlot rt
          var <- readArray vars slot
          !acc <- readPrimVar var
          let !acc' = reducerStep spec acc x
          writePrimVar var acc'

  loopGetReducer redVar =
    Kernel $ \rt ->
      case redVar of
        LocalRedVar var spec -> do
          !acc <- readPrimVar var
          pure (reducerDone spec acc)
        SharedRedVar vars workers spec -> do
          if rtWorkerId rt /= Nothing || rtLoopDepth rt > 0
            then invalidUsage "getReducer may only be used between parallel phases"
            else combineSharedReducer vars workers spec

  loopAccumFor n initial body = Kernel $ \rt ->
    let go !i !acc
          | i >= n = pure acc
          | otherwise = do
              acc' <- runKernel (body acc i) rt
              go (i + 1) acc'
     in go 0 initial

  loopAccumVecFor n initial body =
    Kernel $ \rt ->
      accumVecForIO rt n initial (\acc i -> runKernel (body acc i) rt)

  loopAccumIVecFor n initial body =
    Kernel $ \rt ->
      let go !i acc
            | i >= n = pure acc
            | otherwise = do
                acc' <- runKernel (body acc i) rt
                go (i + 1) acc'
       in go 0 initial

  loopAccumI32VecFor n (I32Vec v0) body =
    Kernel $ \rt ->
      let go !i v
            | i >= n = pure (I32Vec v)
            | otherwise = do
                I32Vec v' <- runKernel (body (I32Vec v) i) rt
                go (i + 1) v'
       in go 0 v0

  loopAccumDVecFor n (DVec lo0 hi0) body =
    Kernel $ \rt ->
      let go !i lo hi
            | i >= n = pure (DVec lo hi)
            | otherwise = do
                DVec lo' hi' <- runKernel (body (DVec lo hi) i) rt
                go (i + 1) lo' hi'
       in go 0 lo0 hi0

  loopNewAcc initial body =
    Kernel $ \rt -> do
      var <- newPrimVar initial
      runKernel (body (AccVar var)) rt

  loopReadAcc (AccVar var) = Kernel (\_ -> readPrimVar var)

  loopWriteAcc (AccVar var) x = Kernel (\_ -> writePrimVar var x)

  {-# INLINE loopFoldFor #-}
  loopFoldFor (Reducer spec) n body = Kernel $ \rt ->
    let go !i !acc
          | i >= n = pure (reducerDone spec acc)
          | otherwise = do
              x <- runKernel (body i) rt
              let !acc' = reducerStep spec acc x
              go (i + 1) acc'
     in go 0 (reducerInit spec)

  {-# INLINE loopParFoldFor #-}
  loopParFoldFor (Reducer spec) n body = Kernel $ \rt ->
    if n <= 0
      then pure (reducerDone spec (reducerInit spec))
      else dispatchFold rt n spec $ \workerRt start end ->
             let go !i !acc
                   | i >= end = pure acc
                   | otherwise = do
                       x <- runKernel (body i) workerRt
                       go (i + 1) $! reducerStep spec acc x
              in go start (reducerInit spec)

class VecAccum a where
  accumVecForIO :: Runtime -> Int -> Vec a -> (Vec a -> Int -> IO (Vec a)) -> IO (Vec a)

genericAccumVecForIO :: Runtime -> Int -> Vec a -> (Vec a -> Int -> IO (Vec a)) -> IO (Vec a)
genericAccumVecForIO _ n initial body =
  let go !i !acc
        | i >= n = pure acc
        | otherwise = do
            acc' <- body acc i
            go (i + 1) acc'
   in go 0 initial

instance {-# OVERLAPPABLE #-} VecOps a => VecAccum a where
  accumVecForIO = genericAccumVecForIO

instance VecAccum Int where
  accumVecForIO _ n initial body =
#if WORD_SIZE_IN_BITS == 64
    case simdOfIntVec initial of
      (# lo0, hi0 #) ->
        let go !i lo hi
              | i >= n = pure (VecInt lo hi)
              | otherwise = do
                  acc <- body (VecInt lo hi) i
                  case simdOfIntVec acc of
                    (# lo', hi' #) ->
                      go (i + 1) lo' hi'
         in go 0 lo0 hi0
#else
    case simdOfIntVec initial of
      v0 ->
        let go !i v
              | i >= n = pure (VecInt v)
              | otherwise = do
                  acc <- body (VecInt v) i
                  case simdOfIntVec acc of
                    v' ->
                      go (i + 1) v'
         in go 0 v0
#endif

instance VecAccum Int32 where
  accumVecForIO _ n initial body =
    case simdOfInt32Vec initial of
      v0 ->
        let go !i v
              | i >= n = pure (VecInt32 v)
              | otherwise = do
                  acc <- body (VecInt32 v) i
                  case simdOfInt32Vec acc of
                    v' ->
                      go (i + 1) v'
         in go 0 v0

instance VecAccum Double where
  accumVecForIO _ n initial body =
    case simdOfDoubleVec initial of
      (# lo0, hi0 #) ->
        let go !i lo hi
              | i >= n = pure (VecDouble lo hi)
              | otherwise = do
                  acc <- body (VecDouble lo hi) i
                  case simdOfDoubleVec acc of
                    (# lo', hi' #) ->
                      go (i + 1) lo' hi'
         in go 0 lo0 hi0

{-# INLINE newArr #-}
-- | Allocate a new uninitialized array of the given length.
newArr :: Prim a => Int -> IO (Arr a)
newArr n = Arr n <$> newPrimArray n

{-# INLINE sizeOfArr #-}
-- | Return the number of elements in an array.
sizeOfArr :: Prim a => Arr a -> Int
sizeOfArr (Arr n _) = n

{-# INLINE readArrIO #-}
-- | Read an array element in 'IO'.
readArrIO :: Prim a => Arr a -> Int -> IO a
readArrIO (Arr _ arr) i = readPrimArray arr i

{-# INLINE writeArrIO #-}
-- | Write an array element in 'IO'.
writeArrIO :: Prim a => Arr a -> Int -> a -> IO ()
writeArrIO (Arr _ arr) i x = writePrimArray arr i x

-- | Allocate and populate an array from a list.
fromList :: Prim a => [a] -> IO (Arr a)
fromList xs = do
  let !n = length xs
  arr <- newArr n
  let go !_ [] = pure arr
      go !i (y : ys) = writeArrIO arr i y >> go (i + 1) ys
  go 0 xs

-- | Freeze the current contents of an array into a list.
toList :: Prim a => Arr a -> IO [a]
toList arr = go 0
  where
    !n = sizeOfArr arr
    go !i
      | i >= n = pure []
      | otherwise = do
          x <- readArrIO arr i
          xs <- go (i + 1)
          pure (x : xs)

{-# INLINE runProg #-}
-- | Run a Loom program in 'IO'.
runProg :: Prog a -> IO a
runProg (Prog m) = runKernel (m pure) (Runtime Nothing Nothing 0)

{-# INLINE ix1 #-}
ix1 :: Int -> Ix1
ix1 = Ix1

{-# INLINE ix2 #-}
ix2 :: Int -> Int -> Ix2
ix2 = Ix2

{-# INLINE ix3 #-}
ix3 :: Int -> Int -> Int -> Ix3
ix3 = Ix3

{-# INLINE ixN #-}
ixN :: [Int] -> IxN
ixN = IxN

{-# INLINE sh1 #-}
sh1 :: Int -> Sh1
sh1 = Sh1

{-# INLINE sh2 #-}
sh2 :: Int -> Int -> Sh2
sh2 = Sh2

{-# INLINE sh3 #-}
sh3 :: Int -> Int -> Int -> Sh3
sh3 = Sh3

{-# INLINE shN #-}
shN :: [Int] -> ShN
shN = ShN

{-# INLINE rect2 #-}
rect2 :: Int -> Int -> Int -> Int -> Rect2
rect2 = Rect2

{-# INLINE rectN #-}
rectN :: [Int] -> [Int] -> RectN
rectN = RectN

{-# INLINE affineN #-}
affineN :: [[Int]] -> [Int] -> AffineN
affineN = AffineN

{-# INLINE affine2 #-}
affine2 :: Int -> Int -> Int -> Int -> Int -> Int -> Affine2
affine2 = Affine2

{-# INLINE unIx1 #-}
unIx1 :: Ix1 -> Int
unIx1 (Ix1 i) = i

{-# INLINE unIx2 #-}
unIx2 :: Ix2 -> (Int, Int)
unIx2 (Ix2 i j) = (i, j)

{-# INLINE unIx3 #-}
unIx3 :: Ix3 -> (Int, Int, Int)
unIx3 (Ix3 i j k) = (i, j, k)

{-# INLINE unIxN #-}
unIxN :: IxN -> [Int]
unIxN (IxN is) = is

{-# INLINE unSh2 #-}
unSh2 :: Sh2 -> (Int, Int)
unSh2 (Sh2 i j) = (i, j)

{-# INLINE unShN #-}
unShN :: ShN -> [Int]
unShN (ShN dims) = dims

{-# INLINE unRect2 #-}
unRect2 :: Rect2 -> (Int, Int, Int, Int)
unRect2 (Rect2 rowLo colLo rowHi colHi) = (rowLo, colLo, rowHi, colHi)

{-# INLINE unRectN #-}
unRectN :: RectN -> ([Int], [Int])
unRectN (RectN lo hi) = (lo, hi)

{-# INLINE withIx2 #-}
withIx2 :: Ix2 -> (Int -> Int -> r) -> r
withIx2 (Ix2 i j) f = f i j

{-# INLINE withIx3 #-}
withIx3 :: Ix3 -> (Int -> Int -> Int -> r) -> r
withIx3 (Ix3 i j k) f = f i j k

{-# INLINE index1 #-}
index1 :: Sh1 -> Ix1 -> Int
index1 (Sh1 _) (Ix1 i) = i

{-# INLINE index2 #-}
index2 :: Sh2 -> Ix2 -> Int
index2 (Sh2 _ n) (Ix2 i j) = i * n + j

{-# INLINE index3 #-}
index3 :: Sh3 -> Ix3 -> Int
index3 (Sh3 _ m n) (Ix3 i j k) = ((i * m) + j) * n + k

{-# INLINE indexN #-}
indexN :: ShN -> IxN -> Int
indexN (ShN dims) (IxN is)
  | length dims /= length is =
      error "indexN requires shape and index ranks to match"
  | otherwise =
      go dims is 0
  where
    go [] [] !acc = acc
    go (_ : _) [] !_ = error "indexN: impossible shape/index mismatch"
    go [] (_ : _) !_ = error "indexN: impossible shape/index mismatch"
    go (dim : restDims) (i : restIx) !acc =
      go restDims restIx (acc * dim + i)

{-# INLINE applyAffineN #-}
applyAffineN :: AffineN -> IxN -> IxN
applyAffineN (AffineN rows offset) (IxN is)
  | length rows /= length offset =
      error "applyAffineN requires matching matrix and offset ranks"
  | any ((/= length is) . length) rows =
      error "applyAffineN requires an affine matrix whose row widths match the input rank"
  | otherwise =
      IxN (zipWith (\row bias -> dotProduct row is + bias) rows offset)

{-# INLINE composeAffineN #-}
composeAffineN :: AffineN -> AffineN -> AffineN
composeAffineN (AffineN outerRows outerOffset) (AffineN innerRows innerOffset)
  | length outerRows /= length outerOffset =
      error "composeAffineN requires a well-formed outer affine transform"
  | length innerRows /= length innerOffset =
      error "composeAffineN requires a well-formed inner affine transform"
  | any ((/= innerRank) . length) outerRows =
      error "composeAffineN requires the outer affine input rank to match the inner affine output rank"
  | any ((/= inputRank) . length) innerRows =
      error "composeAffineN requires the inner affine matrix to have consistent row widths"
  | otherwise =
      AffineN
        [ [sum (zipWith (*) outerRow column) | column <- columnsOf innerRows]
        | outerRow <- outerRows
        ]
        (zipWith (\outerRow bias -> dotProduct outerRow innerOffset + bias) outerRows outerOffset)
  where
    innerRank = length innerRows
    inputRank =
      case innerRows of
        [] -> 0
        row0 : _ -> length row0

{-# INLINE invertAffineN #-}
invertAffineN :: AffineN -> Maybe AffineN
invertAffineN (AffineN rows offset) = do
  invRows <- invertIntegerMatrix rows
  let !bias = map negate (multiplyMatrixVector invRows offset)
  pure (AffineN invRows bias)

{-# INLINE identityAffineN #-}
identityAffineN :: Int -> AffineN
identityAffineN rank =
  AffineN
    [ [if i == j then 1 else 0 | j <- [0 .. rank - 1]]
    | i <- [0 .. rank - 1]
    ]
    (replicate rank 0)

{-# INLINE permuteAffineN #-}
permuteAffineN :: [Int] -> AffineN
permuteAffineN order
  | not (isPermutation order) =
      error "permuteAffineN requires a valid permutation"
  | otherwise =
      AffineN
        [ [if source == axis then 1 else 0 | axis <- [0 .. rank - 1]]
        | source <- order
        ]
        (replicate rank 0)
  where
    rank = length order

{-# INLINE boundingBoxAffineN #-}
boundingBoxAffineN :: AffineN -> RectN -> RectN
boundingBoxAffineN affine rect@(RectN lo _)
  | isEmptyRectN rect = RectN lo lo
  | otherwise =
      let points = map (unIxN . applyAffineN affine . IxN) (rectCornersN rect)
          mins = foldl1 (zipWith min) points
          maxs = foldl1 (zipWith max) points
       in RectN mins (zipWith (+) maxs (repeat 1))

{-# INLINE applyAffine2 #-}
applyAffine2 :: Affine2 -> Ix2 -> Ix2
applyAffine2 (Affine2 a00 a01 a10 a11 b0 b1) (Ix2 i j) =
  Ix2
    (a00 * i + a01 * j + b0)
    (a10 * i + a11 * j + b1)

{-# INLINE composeAffine2 #-}
composeAffine2 :: Affine2 -> Affine2 -> Affine2
composeAffine2
  (Affine2 a00 a01 a10 a11 b0 b1)
  (Affine2 c00 c01 c10 c11 d0 d1) =
    Affine2
      (a00 * c00 + a01 * c10)
      (a00 * c01 + a01 * c11)
      (a10 * c00 + a11 * c10)
      (a10 * c01 + a11 * c11)
      (a00 * d0 + a01 * d1 + b0)
      (a10 * d0 + a11 * d1 + b1)

{-# INLINE invertAffine2 #-}
invertAffine2 :: Affine2 -> Maybe Affine2
invertAffine2 (Affine2 a00 a01 a10 a11 b0 b1) =
  let !det = a00 * a11 - a01 * a10
   in case det of
        1 ->
          let !c00 = a11
              !c01 = negate a01
              !c10 = negate a10
              !c11 = a00
              !d0 = negate (c00 * b0 + c01 * b1)
              !d1 = negate (c10 * b0 + c11 * b1)
           in Just (Affine2 c00 c01 c10 c11 d0 d1)
        (-1) ->
          let !c00 = negate a11
              !c01 = a01
              !c10 = a10
              !c11 = negate a00
              !d0 = negate (c00 * b0 + c01 * b1)
              !d1 = negate (c10 * b0 + c11 * b1)
           in Just (Affine2 c00 c01 c10 c11 d0 d1)
        _ ->
          Nothing

{-# INLINE identityAffine2 #-}
identityAffine2 :: Affine2
identityAffine2 = Affine2 1 0 0 1 0 0

{-# INLINE identityScheduleN #-}
identityScheduleN :: ScheduleN
identityScheduleN = ScheduleN []

{-# INLINE affineScheduleN #-}
affineScheduleN :: AffineN -> ScheduleN
affineScheduleN affine = ScheduleN [ScheduleAffineStageN affine]

{-# INLINE tileScheduleN #-}
tileScheduleN :: [Int] -> ScheduleN
tileScheduleN tileDims = ScheduleN [ScheduleTileStageN tileDims]

{-# INLINE permuteScheduleN #-}
permuteScheduleN :: [Int] -> ScheduleN
permuteScheduleN =
  affineScheduleN . permuteAffineN

{-# INLINE composeScheduleN #-}
composeScheduleN :: ScheduleN -> ScheduleN -> ScheduleN
composeScheduleN (ScheduleN left) (ScheduleN right) =
  ScheduleN (normalizeScheduleStagesN (left ++ right))

renderScheduleN :: ScheduleN -> String
renderScheduleN (ScheduleN stages) =
  case normalizeScheduleStagesN stages of
    [] -> "identity"
    normalized ->
      foldl1 (\acc stageText -> acc ++ " -> " ++ stageText) (map renderStageN normalized)
  where
    renderStageN (ScheduleAffineStageN _) = "affine"
    renderStageN (ScheduleTileStageN tileDims) =
      "tile(" ++ renderIntList tileDims ++ ")"

{-# INLINE identityTransform2D #-}
identityTransform2D :: Transform2D
identityTransform2D = Transform2D TransformIdentity2D

{-# INLINE affineTransform2D #-}
affineTransform2D :: Affine2 -> Transform2D
affineTransform2D affine = Transform2D (TransformAffineStage2D affine)

{-# INLINE tileTransform2D #-}
tileTransform2D :: Int -> Int -> Transform2D
tileTransform2D tileRows tileCols = Transform2D (TransformTileStage2D tileRows tileCols)

{-# INLINE composeTransform2D #-}
composeTransform2D :: Transform2D -> Transform2D -> Transform2D
composeTransform2D (Transform2D left) (Transform2D right) =
  Transform2D (TransformCompose2D left right)

{-# INLINE interchange2D #-}
interchange2D :: Affine2
interchange2D = Affine2 0 1 1 0 0 0

{-# INLINE interchangeTransform2D #-}
interchangeTransform2D :: Transform2D
interchangeTransform2D = affineTransform2D interchange2D

{-# INLINE skew2D #-}
skew2D :: Int -> Affine2
skew2D factor = Affine2 1 0 factor 1 0 0

{-# INLINE skewTransform2D #-}
skewTransform2D :: Int -> Transform2D
skewTransform2D factor = affineTransform2D (skew2D factor)

{-# INLINE applyTransform2D #-}
applyTransform2D ::
  Loop repr =>
  Transform2D ->
  Domain2D ->
  (Domain2D -> (Ix2 -> repr ()) -> repr ()) ->
  (Ix2 -> repr ()) ->
  repr ()
applyTransform2D transform domain k emit =
  let !normalized = compileTransform2D transform
   in applyNormalizedTransform2D normalized domain k emit

{-# INLINE compileTransform2D #-}
compileTransform2D :: Transform2D -> NormalizedTransform2D
compileTransform2D (Transform2D expr) =
  normalizeTransformStages2D (collectTransformStages2D expr [])

{-# INLINE collectTransformStages2D #-}
collectTransformStages2D ::
  TransformExpr2D ->
  [RawTransformStage2D] ->
  [RawTransformStage2D]
collectTransformStages2D TransformIdentity2D stages = stages
collectTransformStages2D (TransformAffineStage2D affine) stages =
  RawAffineStage2D affine : stages
collectTransformStages2D (TransformTileStage2D tileRows tileCols) stages =
  RawTileStage2D tileRows tileCols : stages
collectTransformStages2D (TransformCompose2D left right) stages =
  collectTransformStages2D left (collectTransformStages2D right stages)

{-# INLINE normalizeTransformStages2D #-}
normalizeTransformStages2D :: [RawTransformStage2D] -> NormalizedTransform2D
normalizeTransformStages2D stages =
  NormalizedTransform2D (reverse (go stages identityAffine2 []))
  where
    go [] !pendingAffine acc =
      flushNormalizedAffine2D pendingAffine acc
    go (RawAffineStage2D affine : rest) !pendingAffine acc =
      go rest (composeAffine2 affine pendingAffine) acc
    go (RawTileStage2D tileRows tileCols : rest) !pendingAffine acc =
      go
        rest
        identityAffine2
        (NormalizedTileStage2D tileRows tileCols : flushNormalizedAffine2D pendingAffine acc)

{-# INLINE flushNormalizedAffine2D #-}
flushNormalizedAffine2D ::
  Affine2 ->
  [NormalizedTransformStage2D] ->
  [NormalizedTransformStage2D]
flushNormalizedAffine2D affine acc =
  case compileAffine2D affine of
    CompiledIdentity2D -> acc
    compiledAffine -> NormalizedAffineStage2D compiledAffine : acc

{-# INLINE applyNormalizedTransform2D #-}
applyNormalizedTransform2D ::
  Loop repr =>
  NormalizedTransform2D ->
  Domain2D ->
  (Domain2D -> (Ix2 -> repr ()) -> repr ()) ->
  (Ix2 -> repr ()) ->
  repr ()
applyNormalizedTransform2D (NormalizedTransform2D stages) =
  go stages
  where
    go [] domain k emit = k domain emit
    go (stage : rest) domain k emit =
      applyNormalizedStage2D
        stage
        domain
        (\domain' emit' -> go rest domain' k emit')
        emit

{-# INLINE applyNormalizedStage2D #-}
applyNormalizedStage2D ::
  Loop repr =>
  NormalizedTransformStage2D ->
  Domain2D ->
  (Domain2D -> (Ix2 -> repr ()) -> repr ()) ->
  (Ix2 -> repr ()) ->
  repr ()
applyNormalizedStage2D (NormalizedAffineStage2D affine) domain k emit =
  applyCompiledAffine2D affine domain k emit
applyNormalizedStage2D (NormalizedTileStage2D tileRows tileCols) domain k emit =
  tileDomain2DRepr tileRows tileCols domain (\tileDomain -> k tileDomain emit)

{-# INLINE applyCompiledAffine2D #-}
applyCompiledAffine2D ::
  Loop repr =>
  CompiledAffine2D ->
  Domain2D ->
  (Domain2D -> (Ix2 -> repr ()) -> repr ()) ->
  (Ix2 -> repr ()) ->
  repr ()
applyCompiledAffine2D CompiledIdentity2D domain k emit =
  k domain emit
applyCompiledAffine2D CompiledInterchange2D domain k emit =
  case domain of
    RectDomain rect ->
      k
        (RectDomain (transposeRect2 rect))
        (\ix -> emit (applyAffine2 interchange2D ix))
    SkewDomain _ _ ->
      applyAffineDomain2DRepr interchange2D domain k emit
applyCompiledAffine2D (CompiledSkew2D factor) domain k emit =
  k
    (skewDomain2D factor domain)
    (\ix -> emit (applyAffine2 (Affine2 1 0 (-factor) 1 0 0) ix))
applyCompiledAffine2D (CompiledGenericAffine2D affine) domain k emit =
  applyAffineDomain2DRepr affine domain k emit

{-# INLINE boundingBoxAffine2D #-}
boundingBoxAffine2D :: Affine2 -> Rect2 -> Rect2
boundingBoxAffine2D affine rect@(Rect2 rowLo colLo rowHi colHi)
  | isEmptyRect2 rect = Rect2 rowLo colLo rowLo colLo
  | otherwise =
      let !rowLast = rowHi - 1
          !colLast = colHi - 1
          Ix2 p00Row p00Col = applyAffine2 affine (Ix2 rowLo colLo)
          Ix2 p01Row p01Col = applyAffine2 affine (Ix2 rowLo colLast)
          Ix2 p10Row p10Col = applyAffine2 affine (Ix2 rowLast colLo)
          Ix2 p11Row p11Col = applyAffine2 affine (Ix2 rowLast colLast)
          !rowMin = min p00Row (min p01Row (min p10Row p11Row))
          !rowMax = max p00Row (max p01Row (max p10Row p11Row))
          !colMin = min p00Col (min p01Col (min p10Col p11Col))
          !colMax = max p00Col (max p01Col (max p10Col p11Col))
        in Rect2 rowMin colMin (rowMax + 1) (colMax + 1)

{-# INLINE vecWidth #-}
-- | The logical lane count used by the portable vector APIs.
vecWidth :: Int
vecWidth = 4

{-# INLINE impossibleScalarVec #-}
impossibleScalarVec :: String -> a
impossibleScalarVec name =
      error (name <> ": unexpected SIMD-backed vector in scalar Vec path")

{-# INLINE readVecScalar #-}
readVecScalar :: Prim a => MutablePrimArray RealWorld a -> Int -> IO (Vec a)
readVecScalar arr i = do
  !x0 <- readPrimArray arr i
  !x1 <- readPrimArray arr (i + 1)
  !x2 <- readPrimArray arr (i + 2)
  !x3 <- readPrimArray arr (i + 3)
  pure (Vec x0 x1 x2 x3)

{-# INLINE writeVecScalar #-}
writeVecScalar :: Prim a => MutablePrimArray RealWorld a -> Int -> Vec a -> IO ()
writeVecScalar arr i vec =
  case vec of
    Vec x0 x1 x2 x3 -> do
      writePrimArray arr i x0
      writePrimArray arr (i + 1) x1
      writePrimArray arr (i + 2) x2
      writePrimArray arr (i + 3) x3
    _ ->
      impossibleScalarVec "writeVecScalar"

{-# INLINE broadcastVecScalar #-}
broadcastVecScalar :: a -> Vec a
broadcastVecScalar x = Vec x x x x

{-# INLINE addVecScalar #-}
addVecScalar :: Num a => Vec a -> Vec a -> Vec a
addVecScalar left right =
  case left of
    Vec a0 a1 a2 a3 ->
      case right of
        Vec b0 b1 b2 b3 ->
          Vec (a0 + b0) (a1 + b1) (a2 + b2) (a3 + b3)
        _ ->
          impossibleScalarVec "addVecScalar"
    _ ->
      impossibleScalarVec "addVecScalar"

{-# INLINE mulVecScalar #-}
mulVecScalar :: Num a => Vec a -> Vec a -> Vec a
mulVecScalar left right =
  case left of
    Vec a0 a1 a2 a3 ->
      case right of
        Vec b0 b1 b2 b3 ->
          Vec (a0 * b0) (a1 * b1) (a2 * b2) (a3 * b3)
        _ ->
          impossibleScalarVec "mulVecScalar"
    _ ->
      impossibleScalarVec "mulVecScalar"

{-# INLINE sumVecScalar #-}
sumVecScalar :: Num a => Vec a -> a
sumVecScalar vec =
  case vec of
    Vec x0 x1 x2 x3 ->
      ((x0 + x1) + x2) + x3
    _ ->
      impossibleScalarVec "sumVecScalar"

#if WORD_SIZE_IN_BITS == 64
{-# INLINE simdOfIntVec #-}
simdOfIntVec :: Vec Int -> (# Int64X2#, Int64X2# #)
simdOfIntVec vec =
  case vec of
    VecInt lo hi ->
      (# lo, hi #)
    Vec x0 x1 x2 x3 ->
      (# packInt64X2# (# intToInt64# (case x0 of I# x0# -> x0#)
                        , intToInt64# (case x1 of I# x1# -> x1#)
                        #)
       , packInt64X2# (# intToInt64# (case x2 of I# x2# -> x2#)
                        , intToInt64# (case x3 of I# x3# -> x3#)
                        #)
       #)

{-# INLINE broadcastVecInt #-}
broadcastVecInt :: Int -> Vec Int
broadcastVecInt (I# x#) =
  VecInt
    (broadcastInt64X2# (intToInt64# x#))
    (broadcastInt64X2# (intToInt64# x#))

{-# INLINE addVecInt #-}
addVecInt :: Vec Int -> Vec Int -> Vec Int
addVecInt left right =
  case simdOfIntVec left of
    (# leftLo, leftHi #) ->
      case simdOfIntVec right of
        (# rightLo, rightHi #) ->
          VecInt
            (plusInt64X2# leftLo rightLo)
            (plusInt64X2# leftHi rightHi)

{-# INLINE mulVecInt #-}
mulVecInt :: Vec Int -> Vec Int -> Vec Int
mulVecInt left right =
  case simdOfIntVec left of
    (# leftLo, leftHi #) ->
      case simdOfIntVec right of
        (# rightLo, rightHi #) ->
          VecInt
            (timesInt64X2# leftLo rightLo)
            (timesInt64X2# leftHi rightHi)

{-# INLINE sumVecInt #-}
sumVecInt :: Vec Int -> Int
sumVecInt vec =
  case simdOfIntVec vec of
    (# lo, hi #) ->
      case plusInt64X2# lo hi of
        sums ->
          case unpackInt64X2# sums of
            (# s0#, s1# #) ->
              I# (int64ToInt# s0# +# int64ToInt# s1#)
#else
{-# INLINE simdOfIntVec #-}
simdOfIntVec :: Vec Int -> Int32X4#
simdOfIntVec vec =
  case vec of
    VecInt v ->
      v
    Vec x0 x1 x2 x3 ->
      packInt32X4#
        (# intToInt32# (case x0 of I# x0# -> x0#)
         , intToInt32# (case x1 of I# x1# -> x1#)
         , intToInt32# (case x2 of I# x2# -> x2#)
         , intToInt32# (case x3 of I# x3# -> x3#)
         #)

{-# INLINE broadcastVecInt #-}
broadcastVecInt :: Int -> Vec Int
broadcastVecInt (I# x#) = VecInt (broadcastInt32X4# (intToInt32# x#))

{-# INLINE addVecInt #-}
addVecInt :: Vec Int -> Vec Int -> Vec Int
addVecInt left right =
  VecInt (plusInt32X4# (simdOfIntVec left) (simdOfIntVec right))

{-# INLINE mulVecInt #-}
mulVecInt :: Vec Int -> Vec Int -> Vec Int
mulVecInt left right =
  VecInt (timesInt32X4# (simdOfIntVec left) (simdOfIntVec right))

{-# INLINE sumVecInt #-}
sumVecInt :: Vec Int -> Int
sumVecInt vec =
  case unpackInt32X4# (simdOfIntVec vec) of
    (# x0#, x1#, x2#, x3# #) ->
      I#
        ((((int32ToInt# x0#) +# (int32ToInt# x1#)) +# (int32ToInt# x2#))
           +# (int32ToInt# x3#))
#endif

{-# INLINE readVecInt #-}
readVecInt :: MutablePrimArray RealWorld Int -> Int -> IO (Vec Int)
readVecInt (MutablePrimArray mba#) (I# i#) =
#if WORD_SIZE_IN_BITS == 64
  primitive $ \s0 ->
    case readInt64ArrayAsInt64X2# mba# i# s0 of
      (# s1, lo #) ->
        case readInt64ArrayAsInt64X2# mba# (i# +# 2#) s1 of
          (# s2, hi #) ->
            (# s2, VecInt lo hi #)
#else
  primitive $ \s0 ->
    case readInt32ArrayAsInt32X4# mba# i# s0 of
      (# s1, v #) ->
        (# s1, VecInt v #)
#endif

{-# INLINE writeVecInt #-}
writeVecInt :: MutablePrimArray RealWorld Int -> Int -> Vec Int -> IO ()
writeVecInt (MutablePrimArray mba#) (I# i#) vec =
  case simdOfIntVec vec of
#if WORD_SIZE_IN_BITS == 64
    (# lo, hi #) ->
      primitive $ \s0 ->
        case writeInt64ArrayAsInt64X2# mba# i# lo s0 of
          s1 ->
            case writeInt64ArrayAsInt64X2# mba# (i# +# 2#) hi s1 of
              s2 ->
                (# s2, () #)
#else
    v ->
      primitive $ \s0 ->
        case writeInt32ArrayAsInt32X4# mba# i# v s0 of
          s1 ->
            (# s1, () #)
#endif

{-# INLINE readIVecIO #-}
readIVecIO :: MutablePrimArray RealWorld Int -> Int -> IO IVec
readIVecIO (MutablePrimArray mba#) (I# i#) =
#if WORD_SIZE_IN_BITS == 64
  primitive $ \s0 ->
    case readInt64ArrayAsInt64X2# mba# i# s0 of
      (# s1, lo #) ->
        case readInt64ArrayAsInt64X2# mba# (i# +# 2#) s1 of
          (# s2, hi #) ->
            (# s2, IVec lo hi #)
#else
  primitive $ \s0 ->
    case readInt32ArrayAsInt32X4# mba# i# s0 of
      (# s1, v #) ->
        (# s1, IVec v #)
#endif

{-# INLINE writeIVecIO #-}
writeIVecIO :: MutablePrimArray RealWorld Int -> Int -> IVec -> IO ()
writeIVecIO (MutablePrimArray mba#) (I# i#) vec =
#if WORD_SIZE_IN_BITS == 64
  case vec of
    IVec lo hi ->
      primitive $ \s0 ->
        case writeInt64ArrayAsInt64X2# mba# i# lo s0 of
          s1 ->
            case writeInt64ArrayAsInt64X2# mba# (i# +# 2#) hi s1 of
              s2 ->
                (# s2, () #)
#else
  case vec of
    IVec v ->
      primitive $ \s0 ->
        case writeInt32ArrayAsInt32X4# mba# i# v s0 of
          s1 ->
            (# s1, () #)
#endif

{-# INLINE broadcastIVec #-}
-- | Broadcast one integer across an 'IVec'.
broadcastIVec :: Int -> IVec
broadcastIVec (I# x#) =
#if WORD_SIZE_IN_BITS == 64
  IVec
    (broadcastInt64X2# (intToInt64# x#))
    (broadcastInt64X2# (intToInt64# x#))
#else
  IVec (broadcastInt32X4# (intToInt32# x#))
#endif

{-# INLINE addIVec #-}
-- | Add two integer SIMD vectors elementwise.
addIVec :: IVec -> IVec -> IVec
addIVec left right =
#if WORD_SIZE_IN_BITS == 64
  case left of
    IVec leftLo leftHi ->
      case right of
        IVec rightLo rightHi ->
          IVec
            (plusInt64X2# leftLo rightLo)
            (plusInt64X2# leftHi rightHi)
#else
  case left of
    IVec leftV ->
      case right of
        IVec rightV ->
          IVec (plusInt32X4# leftV rightV)
#endif

{-# INLINE mulIVec #-}
-- | Multiply two integer SIMD vectors elementwise.
mulIVec :: IVec -> IVec -> IVec
mulIVec left right =
#if WORD_SIZE_IN_BITS == 64
  case left of
    IVec leftLo leftHi ->
      case right of
        IVec rightLo rightHi ->
          IVec
            (timesInt64X2# leftLo rightLo)
            (timesInt64X2# leftHi rightHi)
#else
  case left of
    IVec leftV ->
      case right of
        IVec rightV ->
          IVec (timesInt32X4# leftV rightV)
#endif

{-# INLINE sumIVec #-}
-- | Sum the lanes of an integer SIMD vector.
sumIVec :: IVec -> Int
sumIVec vec =
#if WORD_SIZE_IN_BITS == 64
  case vec of
    IVec lo hi ->
      case plusInt64X2# lo hi of
        sums ->
          case unpackInt64X2# sums of
            (# s0#, s1# #) ->
              I# (int64ToInt# s0# +# int64ToInt# s1#)
#else
  case vec of
    IVec v ->
      case unpackInt32X4# v of
        (# x0#, x1#, x2#, x3# #) ->
          I# ((((int32ToInt# x0#) +# (int32ToInt# x1#)) +# (int32ToInt# x2#)) +# (int32ToInt# x3#))
#endif

{-# INLINE simdOfInt32Vec #-}
simdOfInt32Vec :: Vec Int32 -> Int32X4#
simdOfInt32Vec vec =
  case vec of
    VecInt32 v ->
      v
    Vec x0 x1 x2 x3 ->
      packInt32X4#
        (# case x0 of I32# x0# -> x0#
         , case x1 of I32# x1# -> x1#
         , case x2 of I32# x2# -> x2#
         , case x3 of I32# x3# -> x3#
         #)

{-# INLINE broadcastVecInt32 #-}
broadcastVecInt32 :: Int32 -> Vec Int32
broadcastVecInt32 (I32# x#) = VecInt32 (broadcastInt32X4# x#)

{-# INLINE addVecInt32 #-}
addVecInt32 :: Vec Int32 -> Vec Int32 -> Vec Int32
addVecInt32 left right =
  VecInt32 (plusInt32X4# (simdOfInt32Vec left) (simdOfInt32Vec right))

{-# INLINE mulVecInt32 #-}
mulVecInt32 :: Vec Int32 -> Vec Int32 -> Vec Int32
mulVecInt32 left right =
  VecInt32 (timesInt32X4# (simdOfInt32Vec left) (simdOfInt32Vec right))

{-# INLINE sumVecInt32 #-}
sumVecInt32 :: Vec Int32 -> Int32
sumVecInt32 vec =
  case unpackInt32X4# (simdOfInt32Vec vec) of
    (# x0#, x1#, x2#, x3# #) ->
      I32#
        (intToInt32#
           ((((int32ToInt# x0#) +# (int32ToInt# x1#)) +# (int32ToInt# x2#))
              +# (int32ToInt# x3#)))

{-# INLINE readVecInt32 #-}
readVecInt32 :: MutablePrimArray RealWorld Int32 -> Int -> IO (Vec Int32)
readVecInt32 (MutablePrimArray mba#) (I# i#) =
  primitive $ \s0 ->
    case readInt32ArrayAsInt32X4# mba# i# s0 of
      (# s1, v #) ->
        (# s1, VecInt32 v #)

{-# INLINE writeVecInt32 #-}
writeVecInt32 :: MutablePrimArray RealWorld Int32 -> Int -> Vec Int32 -> IO ()
writeVecInt32 (MutablePrimArray mba#) (I# i#) vec =
  primitive $ \s0 ->
    case writeInt32ArrayAsInt32X4# mba# i# (simdOfInt32Vec vec) s0 of
      s1 ->
        (# s1, () #)

{-# INLINE readI32VecIO #-}
readI32VecIO :: MutablePrimArray RealWorld Int32 -> Int -> IO I32Vec
readI32VecIO (MutablePrimArray mba#) (I# i#) =
  primitive $ \s0 ->
    case readInt32ArrayAsInt32X4# mba# i# s0 of
      (# s1, v #) ->
        (# s1, I32Vec v #)

{-# INLINE writeI32VecIO #-}
writeI32VecIO :: MutablePrimArray RealWorld Int32 -> Int -> I32Vec -> IO ()
writeI32VecIO (MutablePrimArray mba#) (I# i#) (I32Vec v) =
  primitive $ \s0 ->
    case writeInt32ArrayAsInt32X4# mba# i# v s0 of
      s1 ->
        (# s1, () #)

{-# INLINE broadcastI32Vec #-}
-- | Broadcast one 'Int32' across an 'I32Vec'.
broadcastI32Vec :: Int32 -> I32Vec
broadcastI32Vec (I32# x#) = I32Vec (broadcastInt32X4# x#)

{-# INLINE addI32Vec #-}
-- | Add two 'Int32' SIMD vectors elementwise.
addI32Vec :: I32Vec -> I32Vec -> I32Vec
addI32Vec (I32Vec leftV) (I32Vec rightV) =
  I32Vec (plusInt32X4# leftV rightV)

{-# INLINE mulI32Vec #-}
-- | Multiply two 'Int32' SIMD vectors elementwise.
mulI32Vec :: I32Vec -> I32Vec -> I32Vec
mulI32Vec (I32Vec leftV) (I32Vec rightV) =
  I32Vec (timesInt32X4# leftV rightV)

{-# INLINE sumI32Vec #-}
-- | Sum the lanes of an 'Int32' SIMD vector.
sumI32Vec :: I32Vec -> Int32
sumI32Vec (I32Vec v) =
  case unpackInt32X4# v of
    (# x0#, x1#, x2#, x3# #) ->
      I32#
        (intToInt32#
           ((((int32ToInt# x0#) +# (int32ToInt# x1#)) +# (int32ToInt# x2#))
              +# (int32ToInt# x3#)))

{-# INLINE simdOfDoubleVec #-}
simdOfDoubleVec :: Vec Double -> (# DoubleX2#, DoubleX2# #)
simdOfDoubleVec vec =
  case vec of
    VecDouble lo hi ->
      (# lo, hi #)
    Vec x0 x1 x2 x3 ->
      (# packDoubleX2# (# case x0 of D# x0# -> x0#
                          , case x1 of D# x1# -> x1#
                          #)
       , packDoubleX2# (# case x2 of D# x2# -> x2#
                          , case x3 of D# x3# -> x3#
                          #)
       #)

{-# INLINE broadcastVecDouble #-}
broadcastVecDouble :: Double -> Vec Double
broadcastVecDouble (D# x#) =
  VecDouble
    (broadcastDoubleX2# x#)
    (broadcastDoubleX2# x#)

{-# INLINE addVecDouble #-}
addVecDouble :: Vec Double -> Vec Double -> Vec Double
addVecDouble left right =
  case simdOfDoubleVec left of
    (# leftLo, leftHi #) ->
      case simdOfDoubleVec right of
        (# rightLo, rightHi #) ->
          VecDouble
            (plusDoubleX2# leftLo rightLo)
            (plusDoubleX2# leftHi rightHi)

{-# INLINE mulVecDouble #-}
mulVecDouble :: Vec Double -> Vec Double -> Vec Double
mulVecDouble left right =
  case simdOfDoubleVec left of
    (# leftLo, leftHi #) ->
      case simdOfDoubleVec right of
        (# rightLo, rightHi #) ->
          VecDouble
            (timesDoubleX2# leftLo rightLo)
            (timesDoubleX2# leftHi rightHi)

{-# INLINE sumVecDouble #-}
sumVecDouble :: Vec Double -> Double
sumVecDouble vec =
  case simdOfDoubleVec vec of
    (# lo, hi #) ->
      case plusDoubleX2# lo hi of
        sums ->
          case unpackDoubleX2# sums of
            (# s0#, s1# #) ->
              D# (s0# +## s1#)

{-# INLINE readVecDouble #-}
readVecDouble :: MutablePrimArray RealWorld Double -> Int -> IO (Vec Double)
readVecDouble (MutablePrimArray mba#) (I# i#) =
  primitive $ \s0 ->
    case readDoubleArrayAsDoubleX2# mba# i# s0 of
      (# s1, lo #) ->
        case readDoubleArrayAsDoubleX2# mba# (i# +# 2#) s1 of
          (# s2, hi #) ->
            (# s2, VecDouble lo hi #)

{-# INLINE writeVecDouble #-}
writeVecDouble :: MutablePrimArray RealWorld Double -> Int -> Vec Double -> IO ()
writeVecDouble (MutablePrimArray mba#) (I# i#) vec =
  case simdOfDoubleVec vec of
    (# lo, hi #) ->
      primitive $ \s0 ->
        case writeDoubleArrayAsDoubleX2# mba# i# lo s0 of
          s1 ->
            case writeDoubleArrayAsDoubleX2# mba# (i# +# 2#) hi s1 of
              s2 ->
                (# s2, () #)

{-# INLINE readDVecIO #-}
readDVecIO :: MutablePrimArray RealWorld Double -> Int -> IO DVec
readDVecIO (MutablePrimArray mba#) (I# i#) =
  primitive $ \s0 ->
    case readDoubleArrayAsDoubleX2# mba# i# s0 of
      (# s1, lo #) ->
        case readDoubleArrayAsDoubleX2# mba# (i# +# 2#) s1 of
          (# s2, hi #) ->
            (# s2, DVec lo hi #)

{-# INLINE writeDVecIO #-}
writeDVecIO :: MutablePrimArray RealWorld Double -> Int -> DVec -> IO ()
writeDVecIO (MutablePrimArray mba#) (I# i#) (DVec lo hi) =
  primitive $ \s0 ->
    case writeDoubleArrayAsDoubleX2# mba# i# lo s0 of
      s1 ->
        case writeDoubleArrayAsDoubleX2# mba# (i# +# 2#) hi s1 of
          s2 ->
            (# s2, () #)

{-# INLINE broadcastDVec #-}
-- | Broadcast one 'Double' across a DVec.
broadcastDVec :: Double -> DVec
broadcastDVec (D# x#) = DVec (broadcastDoubleX2# x#) (broadcastDoubleX2# x#)

{-# INLINE addDVec #-}
-- | Add two 'Double' SIMD vectors elementwise.
addDVec :: DVec -> DVec -> DVec
addDVec (DVec leftLo leftHi) (DVec rightLo rightHi) =
  DVec
    (plusDoubleX2# leftLo rightLo)
    (plusDoubleX2# leftHi rightHi)

{-# INLINE mulDVec #-}
-- | Multiply two 'Double' SIMD vectors elementwise.
mulDVec :: DVec -> DVec -> DVec
mulDVec (DVec leftLo leftHi) (DVec rightLo rightHi) =
  DVec
    (timesDoubleX2# leftLo rightLo)
    (timesDoubleX2# leftHi rightHi)

{-# INLINE sumDVec #-}
-- | Sum the lanes of a DVec.
sumDVec :: DVec -> Double
sumDVec (DVec lo hi) =
  case plusDoubleX2# lo hi of
    sums ->
      case unpackDoubleX2# sums of
        (# s0#, s1# #) ->
          D# (s0# +## s1#)

{-# INLINE subDVec #-}
-- | Subtract two 'Double' SIMD vectors elementwise.
subDVec :: DVec -> DVec -> DVec
subDVec (DVec leftLo leftHi) (DVec rightLo rightHi) =
  DVec
    (minusDoubleX2# leftLo rightLo)
    (minusDoubleX2# leftHi rightHi)

-- | Elementwise reciprocal square root of a 'DVec'.
--
-- GHC lacks a SIMD sqrt primop, so this unpacks each lane, applies
{-# INLINE invSqrtDVec #-}
-- scalar 'sqrtDouble#', and repacks.
invSqrtDVec :: DVec -> DVec
invSqrtDVec (DVec lo hi) =
  let !(# a0#, a1# #) = unpackDoubleX2# lo
      !(# b0#, b1# #) = unpackDoubleX2# hi
      !r0# = 1.0## /## sqrtDouble# a0#
      !r1# = 1.0## /## sqrtDouble# a1#
      !r2# = 1.0## /## sqrtDouble# b0#
      !r3# = 1.0## /## sqrtDouble# b1#
  in DVec
       (packDoubleX2# (# r0#, r1# #))
       (packDoubleX2# (# r2#, r3# #))

-- | Fold over a range of 'DVec'-sized (4-'Double') chunks with a 'DVec'
{-# INLINE foldSimdFor4 #-}
-- accumulator.  Equivalent to 'accumDVecFor'.
foldSimdFor4 :: Int -> DVec -> (DVec -> Int -> Prog DVec) -> Prog DVec
foldSimdFor4 = accumDVecFor

{-# INLINE broadcastVec #-}
-- | Broadcast one scalar value across a generic vector.
broadcastVec :: VecOps a => a -> Vec a
broadcastVec = broadcastVecIO
{-# SPECIALIZE INLINE broadcastVec :: Int -> Vec Int #-}
{-# SPECIALIZE INLINE broadcastVec :: Int32 -> Vec Int32 #-}
{-# SPECIALIZE INLINE broadcastVec :: Double -> Vec Double #-}

{-# INLINE addVec #-}
-- | Add two generic vectors elementwise.
addVec :: VecOps a => Vec a -> Vec a -> Vec a
addVec = addVecIO
{-# SPECIALIZE INLINE addVec :: Vec Int -> Vec Int -> Vec Int #-}
{-# SPECIALIZE INLINE addVec :: Vec Int32 -> Vec Int32 -> Vec Int32 #-}
{-# SPECIALIZE INLINE addVec :: Vec Double -> Vec Double -> Vec Double #-}

{-# INLINE mulVec #-}
-- | Multiply two generic vectors elementwise.
mulVec :: VecOps a => Vec a -> Vec a -> Vec a
mulVec = mulVecIO
{-# SPECIALIZE INLINE mulVec :: Vec Int -> Vec Int -> Vec Int #-}
{-# SPECIALIZE INLINE mulVec :: Vec Int32 -> Vec Int32 -> Vec Int32 #-}
{-# SPECIALIZE INLINE mulVec :: Vec Double -> Vec Double -> Vec Double #-}

{-# INLINE sumVec #-}
-- | Sum all lanes of a generic vector.
sumVec :: VecOps a => Vec a -> a
sumVec = sumVecIO
{-# SPECIALIZE INLINE sumVec :: Vec Int -> Int #-}
{-# SPECIALIZE INLINE sumVec :: Vec Int32 -> Int32 #-}
{-# SPECIALIZE INLINE sumVec :: Vec Double -> Double #-}

{-# INLINE parallel #-}
-- | Mark a program region as an outer parallel section.
parallel :: Prog a -> Prog a
parallel body = Prog $ \k -> loopParallel (unProg body k)

{-# INLINE barrier #-}
-- | Synchronize workers inside a parallel region.
barrier :: Prog ()
barrier = Prog $ \k -> do
  loopBarrier
  k ()

{-# INLINE parFor #-}
-- | Run a one-dimensional parallel loop from @0@ to @n - 1@.
parFor :: Int -> (Int -> Prog ()) -> Prog ()
parFor n body = Prog $ \k -> do
  loopParFor n (\i -> unProg (body i) (\() -> pure ()))
  k ()

{-# INLINE parForSh1 #-}
parForSh1 :: Sh1 -> (Ix1 -> Prog ()) -> Prog ()
parForSh1 (Sh1 n) body = Prog $ \k -> do
  loopParFor n (\i -> unProg (body (Ix1 i)) (\() -> pure ()))
  k ()

{-# INLINE parForSh2 #-}
parForSh2 :: Sh2 -> (Ix2 -> Prog ()) -> Prog ()
parForSh2 (Sh2 n m) body =
  case n of
    I# n# ->
      case m of
        I# m# ->
          loopParForSh2# n# m# (\i# j# -> body (Ix2 (I# i#) (I# j#)))

{-# INLINE parForSh3 #-}
parForSh3 :: Sh3 -> (Ix3 -> Prog ()) -> Prog ()
parForSh3 (Sh3 l m n) body =
  case l of
    I# l# ->
      case m of
        I# m# ->
          case n of
            I# n# ->
              loopParForSh3# l# m# n# (\i# j# k# -> body (Ix3 (I# i#) (I# j#) (I# k#)))

{-# INLINE parForShN #-}
parForShN :: ShN -> (IxN -> Prog ()) -> Prog ()
parForShN (ShN dims) body
  | any (< 0) dims = invalidProgUsage "parForShN requires non-negative extents"
  | otherwise =
      case dims of
        [] ->
          body (IxN [])
        [n] ->
          parForSh1 (Sh1 n) (\ix -> body (IxN [unIx1 ix]))
        [rows, cols] ->
          parForSh2 (Sh2 rows cols) (\ix -> withIx2 ix (\i j -> body (IxN [i, j])))
        [depth, rows, cols] ->
          parForSh3 (Sh3 depth rows cols) (\ix -> withIx3 ix (\i j k -> body (IxN [i, j, k])))
        _ ->
          let !total = product dims
           in parFor total (\linear -> body (IxN (decodeLinearIndexN dims linear)))

{-# INLINE parFor2 #-}
parFor2 :: Int -> Int -> (Int -> Int -> Prog ()) -> Prog ()
parFor2 = loopParForSh2

{-# INLINE parFor3 #-}
parFor3 :: Int -> Int -> Int -> (Int -> Int -> Int -> Prog ()) -> Prog ()
parFor3 = loopParForSh3

{-# INLINE parForRect2D #-}
parForRect2D :: Rect2 -> (Ix2 -> Prog ()) -> Prog ()
parForRect2D (Rect2 rowLo colLo rowHi colHi) body =
  Prog $ \k -> do
    parForRect2DRepr rect (\ix -> unProg (body ix) (\() -> pure ()))
    k ()
  where
    rect = Rect2 rowLo colLo rowHi colHi

-- | Parallel loop over rows with a sequential inner column loop.
--
-- Dispatches one chunk of rows to each parallel worker. Within each worker the
-- inner @j@ loop runs sequentially, letting GHC hoist row-invariant expressions
-- (e.g. @i * cols@) out of the inner loop and enabling tighter code generation
-- than 'parFor2'.
{-# INLINE parForRows #-}
parForRows :: Int -> Int -> (Int -> Int -> Prog ()) -> Prog ()
parForRows rows cols body =
  parFor rows $ \i ->
    Prog $ \k ->
      let go !j
            | j >= cols = k ()
            | otherwise = unProg (body i j) (\() -> go (j + 1))
       in go 0

-- | Like 'parForRows' but iterates over a rectangular subregion.
{-# INLINE parForRowsRect2D #-}
parForRowsRect2D :: Rect2 -> (Int -> Int -> Prog ()) -> Prog ()
parForRowsRect2D (Rect2 rowLo colLo rowHi colHi) body =
  parFor (rowHi - rowLo) $ \di ->
    let !i = rowLo + di
     in Prog $ \k ->
          let go !j
                | j >= colHi = k ()
                | otherwise = unProg (body i j) (\() -> go (j + 1))
           in go colLo

-- | Parallel checkerboard (red-black) loop.
--
-- For each row @i@ in the rectangle, iterates only the columns @j@ where
-- @(i + j) \`rem\` 2 == parity@, stepping by 2. This eliminates the
-- branch-per-element overhead of a full 'parForRect2D' with an inline
-- @if even (i+j)@ guard and halves the number of inner-loop iterations.
--
-- Typical usage for a two-phase red-black stencil:
--
-- @
-- parForCheckerboard 0 (rect2 1 1 (n-1) (n-1)) $ \\i j -> updateCell i j
-- barrier
-- parForCheckerboard 1 (rect2 1 1 (n-1) (n-1)) $ \\i j -> updateCell i j
-- @
-- | Like 'parForRows' but extends to three dimensions: parallelises over all
-- @depth * rows@ outer @(i, j)@ pairs and runs a tight sequential inner loop
-- over @k = 0 .. cols-1@.  The sequential k-loop is a join point in the
-- generated Core, which lets LLVM auto-vectorise it for stride-1 workloads.
{-# INLINE parForSlices #-}
parForSlices :: Int -> Int -> Int -> (Int -> Int -> Int -> Prog ()) -> Prog ()
parForSlices depth rows cols body =
  parFor2 depth rows $ \i j ->
    Prog $ \k ->
      let go !kk
            | kk >= cols = k ()
            | otherwise = unProg (body i j kk) (\() -> go (kk + 1))
       in go 0

{-# INLINE parForCheckerboard #-}
parForCheckerboard :: Int -> Rect2 -> (Int -> Int -> Prog ()) -> Prog ()
parForCheckerboard parity (Rect2 rowLo colLo rowHi colHi) body =
  parFor (rowHi - rowLo) $ \di ->
    let !i = rowLo + di
        !startJ = colLo + (i + colLo + parity) `rem` 2
     in Prog $ \k ->
          let go !j
                | j >= colHi = k ()
                | otherwise = unProg (body i j) (\() -> go (j + 2))
           in go startJ

{-# INLINE parForRectN #-}
parForRectN :: RectN -> (IxN -> Prog ()) -> Prog ()
parForRectN (RectN lo hi) body
  | length lo /= length hi =
      invalidProgUsage "parForRectN requires lower and upper bounds with matching ranks"
  | any (< 0) lo =
      invalidProgUsage "parForRectN requires non-negative lower bounds"
  | any (< 0) extents =
      invalidProgUsage "parForRectN requires upper bounds greater than or equal to lower bounds"
  | otherwise =
      case (lo, hi, extents) of
        ([], [], []) ->
          body (IxN [])
        ([lo0], _, [extent0]) ->
          parForSh1 (Sh1 extent0) (\ix -> body (IxN [lo0 + unIx1 ix]))
        ([rowLo, colLo], [rowHi, colHi], _) ->
          parForRect2D (Rect2 rowLo colLo rowHi colHi) (\ix -> withIx2 ix (\i j -> body (IxN [i, j])))
        ([depthLo, rowLo, colLo], _, [depthCount, rowCount, colCount]) ->
          parForSh3 (Sh3 depthCount rowCount colCount) $ \ix ->
            withIx3 ix $ \i j k ->
              body (IxN [depthLo + i, rowLo + j, colLo + k])
        _ ->
          parForShN (ShN extents) $ \(IxN offset) ->
            body (IxN (zipWith (+) lo offset))
  where
    extents = zipWith (-) hi lo

{-# INLINE parForScheduleN #-}
parForScheduleN :: ScheduleN -> ShN -> (IxN -> Prog ()) -> Prog ()
parForScheduleN schedule shape body =
  applyScheduleN (normalizeScheduleN schedule) (rectOfShapeN shape) body

{-# INLINE parForAffineRect2D #-}
parForAffineRect2D :: Affine2 -> Rect2 -> (Ix2 -> Prog ()) -> Prog ()
parForAffineRect2D affine rect body =
  Prog $ \k -> do
    applyAffineRect2DRepr
      affine
      rect
      parForRect2DRepr
      (\ix -> unProg (body ix) (\() -> pure ()))
    k ()

{-# INLINE parForAffine2D #-}
parForAffine2D :: Affine2 -> Sh2 -> (Ix2 -> Prog ()) -> Prog ()
parForAffine2D affine shape =
  parForAffineRect2D affine (rectOfShape2 shape)

{-# INLINE parForWavefront2D #-}
parForWavefront2D :: Sh2 -> (Ix2 -> Prog ()) -> Prog ()
parForWavefront2D shape body =
  Prog $ \k -> do
    parForWavefrontRect2DRepr
      (rectOfShape2 shape)
      (\ix -> unProg (body ix) (\() -> pure ()))
    k ()

{-# INLINE tileRect2D #-}
tileRect2D :: Int -> Int -> Rect2 -> (Rect2 -> Prog ()) -> Prog ()
tileRect2D tileRows tileCols (Rect2 rowLo colLo rowHi colHi) body =
  Prog $ \k -> do
    tileRect2DRepr tileRows tileCols rect (\tileRect -> unProg (body tileRect) (\() -> pure ()))
    k ()
  where
    rect = Rect2 rowLo colLo rowHi colHi

{-# INLINE tileN #-}
tileN :: [Int] -> ShN -> (IxN -> Prog ()) -> Prog ()
tileN tileDims (ShN dims) body
  | length tileDims /= length dims =
      invalidProgUsage "tileN requires tile extents with the same rank as the shape"
  | any (<= 0) tileDims =
      invalidProgUsage "tileN requires positive tile extents"
  | any (< 0) dims =
      invalidProgUsage "tileN requires non-negative shape extents"
  | otherwise =
      parForShN (ShN (zipWith tileCountInt dims tileDims)) $ \(IxN tileIx) ->
        body (IxN (zipWith (*) tileIx tileDims))

{-# INLINE tiledForRect2D #-}
tiledForRect2D :: Int -> Int -> Rect2 -> (Ix2 -> Prog ()) -> Prog ()
tiledForRect2D tileRows tileCols rect body =
  tileRect2D tileRows tileCols rect $ \tileRect ->
    parForRect2D tileRect body

{-# INLINE parForTransform2D #-}
parForTransform2D :: Transform2D -> Sh2 -> (Ix2 -> Prog ()) -> Prog ()
parForTransform2D transform shape body =
  Prog $ \k -> do
    applyTransform2D transform
      (RectDomain (rectOfShape2 shape))
      parForDomain2DRepr
      (\ix -> unProg (body ix) (\() -> pure ()))
    k ()

{-# INLINE tile2D #-}
tile2D :: Int -> Int -> Sh2 -> (Int -> Int -> Prog ()) -> Prog ()
tile2D tileRows tileCols (Sh2 rows cols) body =
  case tileRows of
    I# tileRows# ->
      case tileCols of
        I# tileCols# ->
          case rows of
            I# rows# ->
              case cols of
                I# cols# ->
                  case tileRows# <=# 0# of
                    1# -> invalidProgUsage "tile2D requires a positive row tile size"
                    _ ->
                      case tileCols# <=# 0# of
                        1# -> invalidProgUsage "tile2D requires a positive column tile size"
                        _ ->
                          case rows# <=# 0# of
                            1# -> pure ()
                            _ ->
                              case cols# <=# 0# of
                                1# -> pure ()
                                _ ->
                                   loopParForSh2#
                                     (tileCount# rows# tileRows#)
                                     (tileCount# cols# tileCols#)
                                     (\tileI# tileJ# ->
                                        body
                                          (I# (tileI# *# tileRows#))
                                          (I# (tileJ# *# tileCols#)))

{-# INLINE tile3D #-}
tile3D :: Int -> Int -> Int -> Sh3 -> (Int -> Int -> Int -> Prog ()) -> Prog ()
tile3D tileDepth tileRows tileCols (Sh3 depth rows cols) body =
  case tileDepth of
    I# tileDepth# ->
      case tileRows of
        I# tileRows# ->
          case tileCols of
            I# tileCols# ->
              case depth of
                I# depth# ->
                  case rows of
                    I# rows# ->
                      case cols of
                        I# cols# ->
                          case tileDepth# <=# 0# of
                            1# -> invalidProgUsage "tile3D requires a positive depth tile size"
                            _ ->
                              case tileRows# <=# 0# of
                                1# -> invalidProgUsage "tile3D requires a positive row tile size"
                                _ ->
                                  case tileCols# <=# 0# of
                                    1# -> invalidProgUsage "tile3D requires a positive column tile size"
                                    _ ->
                                      case depth# <=# 0# of
                                        1# -> pure ()
                                        _ ->
                                          case rows# <=# 0# of
                                            1# -> pure ()
                                            _ ->
                                              case cols# <=# 0# of
                                                1# -> pure ()
                                                _ ->
                                                  loopParForSh3#
                                                    (tileCount# depth# tileDepth#)
                                                    (tileCount# rows# tileRows#)
                                                    (tileCount# cols# tileCols#)
                                                    (\tileDepthIx# tileRowIx# tileColIx# ->
                                                       body
                                                         (I# (tileDepthIx# *# tileDepth#))
                                                         (I# (tileRowIx# *# tileRows#))
                                                         (I# (tileColIx# *# tileCols#)))

{-# INLINE parForTile2D #-}
parForTile2D :: Int -> Int -> Int -> Int -> Sh2 -> (Int -> Int -> Prog ()) -> Prog ()
parForTile2D tileRows tileCols row0 col0 (Sh2 rows cols) body =
  case tileRows of
    I# tileRows# ->
      case tileCols of
        I# tileCols# ->
          case row0 of
            I# row0# ->
              case col0 of
                I# col0# ->
                  case rows of
                    I# rows# ->
                      case cols of
                        I# cols# ->
                          case tileRows# <=# 0# of
                            1# -> invalidProgUsage "parForTile2D requires a positive row tile size"
                            _ ->
                              case tileCols# <=# 0# of
                                1# -> invalidProgUsage "parForTile2D requires a positive column tile size"
                                _ ->
                                  case row0# <# 0# of
                                    1# -> invalidProgUsage "parForTile2D requires a non-negative row origin"
                                    _ ->
                                      case col0# <# 0# of
                                        1# -> invalidProgUsage "parForTile2D requires a non-negative column origin"
                                        _ ->
                                          let !rowCount# = tileSpan# rows# row0# tileRows#
                                              !colCount# = tileSpan# cols# col0# tileCols#
                                           in case rowCount# <=# 0# of
                                                1# -> pure ()
                                                _ ->
                                                  case colCount# <=# 0# of
                                                    1# -> pure ()
                                                    _ ->
                                                      loopParForSh2#
                                                        rowCount#
                                                        colCount#
                                                       (\i# j# ->
                                                          body
                                                            (I# (row0# +# i#))
                                                            (I# (col0# +# j#)))

{-# INLINE parForTile3D #-}
parForTile3D ::
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Sh3 ->
  (Int -> Int -> Int -> Prog ()) ->
  Prog ()
parForTile3D tileDepth tileRows tileCols depth0 row0 col0 (Sh3 depth rows cols) body =
  case tileDepth of
    I# tileDepth# ->
      case tileRows of
        I# tileRows# ->
          case tileCols of
            I# tileCols# ->
              case depth0 of
                I# depth0# ->
                  case row0 of
                    I# row0# ->
                      case col0 of
                        I# col0# ->
                          case depth of
                            I# depth# ->
                              case rows of
                                I# rows# ->
                                  case cols of
                                    I# cols# ->
                                      case tileDepth# <=# 0# of
                                        1# -> invalidProgUsage "parForTile3D requires a positive depth tile size"
                                        _ ->
                                          case tileRows# <=# 0# of
                                            1# -> invalidProgUsage "parForTile3D requires a positive row tile size"
                                            _ ->
                                              case tileCols# <=# 0# of
                                                1# -> invalidProgUsage "parForTile3D requires a positive column tile size"
                                                _ ->
                                                  case depth0# <# 0# of
                                                    1# -> invalidProgUsage "parForTile3D requires a non-negative depth origin"
                                                    _ ->
                                                      case row0# <# 0# of
                                                        1# -> invalidProgUsage "parForTile3D requires a non-negative row origin"
                                                        _ ->
                                                          case col0# <# 0# of
                                                            1# -> invalidProgUsage "parForTile3D requires a non-negative column origin"
                                                            _ ->
                                                              let !depthCount# = tileSpan# depth# depth0# tileDepth#
                                                                  !rowCount# = tileSpan# rows# row0# tileRows#
                                                                  !colCount# = tileSpan# cols# col0# tileCols#
                                                               in case depthCount# <=# 0# of
                                                                    1# -> pure ()
                                                                    _ ->
                                                                      case rowCount# <=# 0# of
                                                                        1# -> pure ()
                                                                        _ ->
                                                                          case colCount# <=# 0# of
                                                                            1# -> pure ()
                                                                            _ ->
                                                                              loopParForSh3#
                                                                                depthCount#
                                                                                rowCount#
                                                                                colCount#
                                                                               (\i# j# k# ->
                                                                                  body
                                                                                    (I# (depth0# +# i#))
                                                                                    (I# (row0# +# j#))
                                                                                    (I# (col0# +# k#)))

{-# INLINE parForTileN #-}
parForTileN :: [Int] -> IxN -> ShN -> (IxN -> Prog ()) -> Prog ()
parForTileN tileDims (IxN origin) (ShN dims) body
  | length tileDims /= length dims =
      invalidProgUsage "parForTileN requires tile extents with the same rank as the shape"
  | length origin /= length dims =
      invalidProgUsage "parForTileN requires an origin with the same rank as the shape"
  | any (<= 0) tileDims =
      invalidProgUsage "parForTileN requires positive tile extents"
  | any (< 0) origin =
      invalidProgUsage "parForTileN requires non-negative tile origins"
  | any (< 0) dims =
      invalidProgUsage "parForTileN requires non-negative shape extents"
  | otherwise =
      parForRectN (RectN origin (zipWith3 tileUpperBound dims origin tileDims)) body

{-# INLINE tiledForN #-}
tiledForN :: [Int] -> ShN -> (IxN -> Prog ()) -> Prog ()
tiledForN tileDims shape@(ShN dims) body
  | length tileDims /= length dims =
      invalidProgUsage "tiledForN requires tile extents with the same rank as the shape"
  | otherwise =
      tileN tileDims shape $ \origin ->
        parForTileN tileDims origin shape body

{-# INLINE tiledFor2D #-}
tiledFor2D :: Int -> Int -> Sh2 -> (Int -> Int -> Prog ()) -> Prog ()
tiledFor2D tileRows tileCols shape body =
  tile2D tileRows tileCols shape $ \row0 col0 ->
    parForTile2D tileRows tileCols row0 col0 shape body

{-# INLINE tiledFor3D #-}
tiledFor3D :: Int -> Int -> Int -> Sh3 -> (Int -> Int -> Int -> Prog ()) -> Prog ()
tiledFor3D tileDepth tileRows tileCols shape body =
  tile3D tileDepth tileRows tileCols shape $ \depth0 row0 col0 ->
    parForTile3D tileDepth tileRows tileCols depth0 row0 col0 shape body

{-# INLINE stripMine #-}
-- | Split a one-dimensional loop into a full-width body and a tail body.
stripMine :: Int -> Int -> (Int -> Prog ()) -> (Int -> Prog ()) -> Prog ()
stripMine width n fullBody tailBody
  | width <= 0 = invalidProgUsage "stripMine requires a positive chunk width"
  | n <= 0 = pure ()
  | otherwise = do
      parFor fullIters $ \chunk ->
        fullBody (chunk * width)
      parFor tailCount $ \offset ->
        tailBody (tailStart + offset)
  where
    !fullIters = n `quot` width
    !tailStart = fullIters * width
    !tailCount = n - tailStart

{-# INLINE readArr #-}
-- | Read an array element inside a Loom program.
readArr :: Prim a => Arr a -> Int -> Prog a
readArr arr i = Prog $ \k -> loopReadArr arr i >>= k

{-# INLINE writeArr #-}
-- | Write an array element inside a Loom program.
writeArr :: Prim a => Arr a -> Int -> a -> Prog ()
writeArr arr i x = Prog $ \k -> do
  loopWriteArr arr i x
  k ()

{-# INLINE readVec #-}
-- | Read a generic vector value starting at the given array offset.
readVec :: VecOps a => Arr a -> Int -> Prog (Vec a)
readVec arr i = Prog $ \k -> loopReadVec arr i >>= k
{-# SPECIALIZE INLINE readVec :: Arr Int -> Int -> Prog (Vec Int) #-}
{-# SPECIALIZE INLINE readVec :: Arr Int32 -> Int -> Prog (Vec Int32) #-}
{-# SPECIALIZE INLINE readVec :: Arr Double -> Int -> Prog (Vec Double) #-}

{-# INLINE readIVec #-}
-- | Read an integer SIMD vector starting at the given array offset.
readIVec :: Arr Int -> Int -> Prog IVec
readIVec arr i = Prog $ \k -> loopReadIVec arr i >>= k

{-# INLINE readI32Vec #-}
-- | Read an 'Int32' SIMD vector starting at the given array offset.
readI32Vec :: Arr Int32 -> Int -> Prog I32Vec
readI32Vec arr i = Prog $ \k -> loopReadI32Vec arr i >>= k

{-# INLINE readDVec #-}
-- | Read a 'Double' SIMD vector starting at the given array offset.
readDVec :: Arr Double -> Int -> Prog DVec
readDVec arr i = Prog $ \k -> loopReadDVec arr i >>= k

{-# INLINE writeVec #-}
-- | Write a generic vector value starting at the given array offset.
writeVec :: VecOps a => Arr a -> Int -> Vec a -> Prog ()
writeVec arr i vec = Prog $ \k -> do
  loopWriteVec arr i vec
  k ()
{-# SPECIALIZE INLINE writeVec :: Arr Int -> Int -> Vec Int -> Prog () #-}
{-# SPECIALIZE INLINE writeVec :: Arr Int32 -> Int -> Vec Int32 -> Prog () #-}
{-# SPECIALIZE INLINE writeVec :: Arr Double -> Int -> Vec Double -> Prog () #-}

{-# INLINE writeIVec #-}
-- | Write an integer SIMD vector starting at the given array offset.
writeIVec :: Arr Int -> Int -> IVec -> Prog ()
writeIVec arr i vec = Prog $ \k -> do
  loopWriteIVec arr i vec
  k ()

{-# INLINE writeI32Vec #-}
-- | Write an 'Int32' SIMD vector starting at the given array offset.
writeI32Vec :: Arr Int32 -> Int -> I32Vec -> Prog ()
writeI32Vec arr i vec = Prog $ \k -> do
  loopWriteI32Vec arr i vec
  k ()

{-# INLINE writeDVec #-}
-- | Write a 'Double' SIMD vector starting at the given array offset.
writeDVec :: Arr Double -> Int -> DVec -> Prog ()
writeDVec arr i vec = Prog $ \k -> do
  loopWriteDVec arr i vec
  k ()

{-# INLINE newReducer #-}
-- | Allocate a reducer variable for the duration of a program region.
newReducer :: Reducer a -> (RedVar a -> Prog r) -> Prog r
newReducer reducer body = Prog $ \k ->
  loopNewReducer reducer (\redVar -> unProg (body redVar) k)

{-# INLINE reduce #-}
-- | Contribute one value to a reducer.
reduce :: RedVar a -> a -> Prog ()
reduce redVar x = Prog $ \k -> do
  loopReduce redVar x
  k ()

{-# INLINE getReducer #-}
-- | Read the final value of a reducer.
getReducer :: RedVar a -> Prog a
getReducer redVar = Prog $ \k -> loopGetReducer redVar >>= k

{-# INLINE accumFor #-}
-- | Run a sequential accumulation loop in a program.
accumFor :: Int -> a -> (a -> Int -> Prog a) -> Prog a
accumFor n initial body =
  Prog $ \k -> loopAccumFor n initial (\acc i -> unProg (body acc i) pure) >>= k

{-# INLINE accumVecFor #-}
-- | Run a sequential accumulation loop over generic vector state.
accumVecFor :: VecAccum a => Int -> Vec a -> (Vec a -> Int -> Prog (Vec a)) -> Prog (Vec a)
accumVecFor n initial body =
  Prog $ \k -> loopAccumVecFor n initial (\acc i -> unProg (body acc i) pure) >>= k

{-# INLINE accumIVecFor #-}
-- | Run a sequential accumulation loop over integer SIMD state.
accumIVecFor :: Int -> IVec -> (IVec -> Int -> Prog IVec) -> Prog IVec
accumIVecFor n initial body =
  Prog $ \k -> loopAccumIVecFor n initial (\acc i -> unProg (body acc i) pure) >>= k

{-# INLINE accumI32VecFor #-}
-- | Run a sequential accumulation loop over 'Int32' SIMD state.
accumI32VecFor :: Int -> I32Vec -> (I32Vec -> Int -> Prog I32Vec) -> Prog I32Vec
accumI32VecFor n initial body =
  Prog $ \k -> loopAccumI32VecFor n initial (\acc i -> unProg (body acc i) pure) >>= k

{-# INLINE accumDVecFor #-}
-- | Run a sequential accumulation loop over 'Double' SIMD state.
accumDVecFor :: Int -> DVec -> (DVec -> Int -> Prog DVec) -> Prog DVec
accumDVecFor n initial body =
  Prog $ \k -> loopAccumDVecFor n initial (\acc i -> unProg (body acc i) pure) >>= k

{-# INLINE newAcc #-}
-- | Allocate a mutable scalar accumulator for the duration of a program region.
newAcc :: Prim a => a -> (AccVar a -> Prog r) -> Prog r
newAcc initial body = Prog $ \k ->
  loopNewAcc initial (\accVar -> unProg (body accVar) k)

{-# INLINE readAcc #-}
-- | Read the current value of an accumulator.
readAcc :: Prim a => AccVar a -> Prog a
readAcc accVar = Prog $ \k -> loopReadAcc accVar >>= k

{-# INLINE writeAcc #-}
-- | Overwrite the current value of an accumulator.
writeAcc :: Prim a => AccVar a -> a -> Prog ()
writeAcc accVar x = Prog $ \k -> do
  loopWriteAcc accVar x
  k ()

{-# INLINE foldFor #-}
-- | Reduce the values produced by a loop with the given reducer.
foldFor :: Reducer a -> Int -> (Int -> Prog a) -> Prog a
foldFor reducer n body =
  Prog $ \k -> loopFoldFor reducer n (\i -> unProg (body i) pure) >>= k

{-# INLINE parFoldFor #-}
-- | Parallel version of 'foldFor'.
--
-- Splits the range across workers; each worker accumulates its partition into
-- an unboxed local accumulator, then the partial results are combined with
-- 'reducerMerge'. This is the preferred path for parallel reductions because
-- each worker's accumulator can be register-allocated rather than
-- heap-allocated.
--
-- 'parFoldFor' may only be called between parallel phases (i.e. inside a
-- 'parallel' block but not inside a 'parFor' body).
parFoldFor :: Reducer a -> Int -> (Int -> Prog a) -> Prog a
parFoldFor reducer n body =
  Prog $ \k -> loopParFoldFor reducer n (\i -> unProg (body i) pure) >>= k

{-# INLINE mkReducer #-}
-- | Build a reducer from an initial state, a step function, and a finalizer.
mkReducer :: Prim rep => rep -> (rep -> a -> rep) -> (rep -> a) -> Reducer a
mkReducer initial step done = mkReducerWith initial step merge done
  where
    merge !left !right = step left (done right)

{-# INLINE mkReducerWith #-}
-- | Build a reducer with an explicit merge function.
mkReducerWith ::
  Prim rep =>
  rep ->
  (rep -> a -> rep) ->
  (rep -> rep -> rep) ->
  (rep -> a) ->
  Reducer a
mkReducerWith initial step merge done =
  Reducer
    ReducerSpec
      { reducerInit = initial
      , reducerStep = step
      , reducerMerge = merge
      , reducerDone = done
      }

{-# INLINE intSum #-}
-- | Sum integer values.
intSum :: Reducer Int
intSum = mkReducerWith 0 step merge id
  where
    step !acc !x = acc + x
    merge !left !right = left + right

{-# INLINE int32Sum #-}
-- | Sum 'Int32' values.
int32Sum :: Reducer Int32
int32Sum = mkReducerWith 0 step merge id
  where
    step !acc !x = acc + x
    merge !left !right = left + right

{-# INLINE doubleSum #-}
-- | Sum 'Double' values.
doubleSum :: Reducer Double
doubleSum = mkReducerWith 0 step merge id
  where
    step !acc !x = acc + x
    merge !left !right = left + right

{-# INLINE loopParForSh2 #-}
loopParForSh2 :: Int -> Int -> (Int -> Int -> Prog ()) -> Prog ()
loopParForSh2 (I# rows#) (I# cols#) body =
  loopParForSh2# rows# cols# (\i# j# -> body (I# i#) (I# j#))

{-# INLINE loopParForSh3 #-}
loopParForSh3 :: Int -> Int -> Int -> (Int -> Int -> Int -> Prog ()) -> Prog ()
loopParForSh3 (I# depth#) (I# rows#) (I# cols#) body =
  loopParForSh3# depth# rows# cols# (\i# j# k# -> body (I# i#) (I# j#) (I# k#))

{-# INLINE loopParForSh2# #-}
loopParForSh2# :: Int# -> Int# -> (Int# -> Int# -> Prog ()) -> Prog ()
loopParForSh2# rows# cols# body =
  Prog $ \k -> do
    loopParFor2# rows# cols# (\i# j# -> unProg (body i# j#) (\() -> pure ()))
    k ()

{-# INLINE loopParForSh3# #-}
loopParForSh3# :: Int# -> Int# -> Int# -> (Int# -> Int# -> Int# -> Prog ()) -> Prog ()
loopParForSh3# depth# rows# cols# body =
  Prog $ \k -> do
    loopParFor3# depth# rows# cols# (\i# j# k# -> unProg (body i# j# k#) (\() -> pure ()))
    k ()

{-# INLINE tileCount# #-}
tileCount# :: Int# -> Int# -> Int#
tileCount# len# tile# = quotInt# (len# +# (tile# -# 1#)) tile#

{-# INLINE tileSpan# #-}
tileSpan# :: Int# -> Int# -> Int# -> Int#
tileSpan# len# start# tile# =
  let !remaining# = len# -# start#
   in case remaining# <# tile# of
        1# -> remaining#
        _ -> tile#

{-# INLINE canRunParallelLoop #-}
canRunParallelLoop :: Runtime -> Int -> Bool
canRunParallelLoop rt workers =
  rtLoopDepth rt == 0
    && rtWorkerId rt == Nothing
    && workers > 1

{-# INLINE dispatchLoop #-}
dispatchLoop :: Runtime -> Int -> (Runtime -> Int -> Int -> IO ()) -> IO ()
dispatchLoop rt total runChunk
  | total <= 0 = pure ()
  | otherwise =
      let !childRt = rt {rtLoopDepth = rtLoopDepth rt + 1}
       in case rtTeam rt of
            Just team ->
              if canRunParallelLoop rt (teamWorkerCount team)
                then runInParallel team childRt
                else runChunk childRt 0 total
            Nothing -> do
              caps <- getNumCapabilities
              let !workers = min total (max 1 caps)
              if canRunParallelLoop rt workers
                then
                  withGlobalTeam workers $ \team ->
                    runInParallel team (childRt {rtTeam = Just team})
                else runChunk childRt 0 total
  where
    runInParallel !team !childRt =
      runParallelPhaseStatic team total $ \workerId start end ->
        let !workerRt = childRt {rtTeam = Just team, rtWorkerId = Just workerId}
         in runChunk workerRt start end

-- | Like 'dispatchLoop' but each worker runs a fold that returns a partial
-- accumulator. After all workers finish, partial results are combined using
-- 'reducerMerge' and finalised with 'reducerDone'.
{-# INLINE dispatchFold #-}
dispatchFold ::
  Prim rep =>
  Runtime ->
  Int ->
  ReducerSpec rep a ->
  (Runtime -> Int -> Int -> IO rep) ->
  IO a
dispatchFold rt total spec runChunk
  | total <= 0 = pure (reducerDone spec (reducerInit spec))
  | otherwise =
      let !childRt = rt {rtLoopDepth = rtLoopDepth rt + 1}
       in case rtTeam rt of
            Just team ->
              if canRunParallelLoop rt (teamWorkerCount team)
                then runParallelFold team childRt
                else do
                  rep <- runChunk childRt 0 total
                  pure (reducerDone spec rep)
            Nothing -> do
              caps <- getNumCapabilities
              let !workers = min total (max 1 caps)
              if canRunParallelLoop rt workers
                then
                  withParallelTeam workers $ \team ->
                    runParallelFold team (childRt {rtTeam = Just team})
                else do
                  rep <- runChunk childRt 0 total
                  pure (reducerDone spec rep)
  where
    runParallelFold !team !childRt = do
      let !workers = teamWorkerCount team
      partials <- newPrimArray workers
      let initSlots !i
            | i >= workers = pure ()
            | otherwise = do
                writePrimArray partials i (reducerInit spec)
                initSlots (i + 1)
      initSlots 0
      runParallelPhaseStatic team total $ \workerId start end -> do
        let !workerRt = childRt {rtTeam = Just team, rtWorkerId = Just workerId}
        chunkRep <- runChunk workerRt start end
        writePrimArray partials workerId chunkRep
      let mergeAll !acc !i
            | i >= workers = pure (reducerDone spec acc)
            | otherwise = do
                partial <- readPrimArray partials i
                mergeAll (reducerMerge spec acc partial) (i + 1)
      mergeAll (reducerInit spec) 0

{-# INLINE runLinear2D #-}
runLinear2D ::
  Runtime ->
  Int# ->
  (Int# -> Int# -> Kernel ()) ->
  Int ->
  Int ->
  IO ()
runLinear2D !workerRt m# body start end =
  case start of
    I# start# ->
      case end of
        I# end# ->
          case start# >=# end# of
            1# -> pure ()
            _ ->
              let !i0# = quotInt# start# m#
                  !j0# = start# -# (i0# *# m#)
               in go start# end# i0# j0#
  where
    go !idx# !end# !i# !j# =
      case idx# >=# end# of
        1# -> pure ()
        _ -> do
          runKernel (body i# j#) workerRt
          let !j'# = j# +# 1#
              !idx'# = idx# +# 1#
          case j'# <# m# of
            1# -> go idx'# end# i# j'#
            _ -> go idx'# end# (i# +# 1#) 0#

{-# INLINE runLinear3D #-}
runLinear3D ::
  Runtime ->
  Int# ->
  Int# ->
  (Int# -> Int# -> Int# -> Kernel ()) ->
  Int ->
  Int ->
  IO ()
runLinear3D !workerRt m# p# body start end =
  case start of
    I# start# ->
      case end of
        I# end# ->
          case start# >=# end# of
            1# -> pure ()
            _ ->
              let !plane# = m# *# p#
                  !i0# = quotInt# start# plane#
                  !rem0# = start# -# (i0# *# plane#)
                  !j0# = quotInt# rem0# p#
                  !k0# = rem0# -# (j0# *# p#)
               in go start# end# i0# j0# k0#
  where
    go !idx# !end# !i# !j# !k# =
      case idx# >=# end# of
        1# -> pure ()
        _ -> do
          runKernel (body i# j# k#) workerRt
          let !k'# = k# +# 1#
              !idx'# = idx# +# 1#
          case k'# <# p# of
            1# -> go idx'# end# i# j# k'#
            _ ->
              let !j'# = j# +# 1#
               in case j'# <# m# of
                    1# -> go idx'# end# i# j'# 0#
                    _ -> go idx'# end# (i# +# 1#) 0# 0#

shouldShareReducer :: Runtime -> Bool
shouldShareReducer rt =
  case (rtTeam rt, rtWorkerId rt, rtLoopDepth rt) of
    (Just _, Nothing, 0) -> True
    _ -> False

parallelWorkers :: Runtime -> Int
parallelWorkers rt =
  case rtTeam rt of
    Just team -> teamWorkerCount team
    Nothing -> 1

reducerSlot :: Runtime -> Int
reducerSlot rt =
  case rtWorkerId rt of
    Just workerId -> workerId
    Nothing -> 0

newLocalRedVar :: Reducer a -> IO (RedVar a)
newLocalRedVar (Reducer spec) = do
  var <- newPrimVar (reducerInit spec)
  pure (LocalRedVar var spec)

newSharedRedVar :: Int -> Reducer a -> IO (RedVar a)
newSharedRedVar workers (Reducer spec) = do
  let !count = max 1 workers
  firstVar <- newPrimVar (reducerInit spec)
  vars <- newArray count firstVar
  let fill !i
        | i >= count = pure ()
        | otherwise = do
            var <- newPrimVar (reducerInit spec)
            writeArray vars i var
            fill (i + 1)
  fill 1
  pure (SharedRedVar vars count spec)

combineSharedReducer ::
  Prim rep =>
  MutableArray RealWorld (PrimVar RealWorld rep) ->
  Int ->
  ReducerSpec rep a ->
  IO a
combineSharedReducer vars workers spec = do
  firstVar <- readArray vars 0
  !firstAcc <- readPrimVar firstVar
  let go !i !acc
        | i >= workers = pure (reducerDone spec acc)
        | otherwise = do
            var <- readArray vars i
            !partial <- readPrimVar var
            let !acc' = reducerMerge spec acc partial
            go (i + 1) acc'
  go 1 firstAcc

invalidUsage :: String -> IO a
invalidUsage = throwIO . userError

{-# INLINE invalidUsageRepr #-}
invalidUsageRepr :: String -> repr a
invalidUsageRepr = error

invalidProgUsage :: String -> Prog a
invalidProgUsage msg = Prog $ \_ -> error msg

{-# INLINE decodeLinearIndexN #-}
decodeLinearIndexN :: [Int] -> Int -> [Int]
decodeLinearIndexN dims linear =
  go (reverse dims) linear []
  where
    go [] !_ acc = acc
    go (dim : rest) !n acc =
      let (!q, !r) = quotRem n dim
       in go rest q (r : acc)

{-# INLINE tileCountInt #-}
tileCountInt :: Int -> Int -> Int
tileCountInt n width
  | n <= 0 = 0
  | otherwise = ((n - 1) `quot` width) + 1

{-# INLINE tileUpperBound #-}
tileUpperBound :: Int -> Int -> Int -> Int
tileUpperBound extent origin width =
  min extent (origin + width)

{-# INLINE dotProduct #-}
dotProduct :: [Int] -> [Int] -> Int
dotProduct xs ys = sum (zipWith (*) xs ys)

{-# INLINE multiplyMatrixVector #-}
multiplyMatrixVector :: [[Int]] -> [Int] -> [Int]
multiplyMatrixVector rows vec =
  map (`dotProduct` vec) rows

{-# INLINE columnsOf #-}
columnsOf :: [[Int]] -> [[Int]]
columnsOf [] = []
columnsOf rows@(row0 : _)
  | all ((== length row0) . length) rows =
      [ [row !! i | row <- rows] | i <- [0 .. length row0 - 1] ]
  | otherwise =
      error "columnsOf requires rectangular matrices"

rectCornersN :: RectN -> [[Int]]
rectCornersN rect@(RectN lo hi)
  | isEmptyRectN rect = []
  | otherwise =
      sequence [[lower, upper - 1] | (lower, upper) <- zip lo hi]

renderIntList :: [Int] -> String
renderIntList xs =
  intercalate "," (map show xs)

isPermutation :: [Int] -> Bool
isPermutation xs =
  sort xs == [0 .. length xs - 1]

invertIntegerMatrix :: [[Int]] -> Maybe [[Int]]
invertIntegerMatrix rows
  | null rows = Just []
  | any ((/= rank) . length) rows = Nothing
  | otherwise = go 0 augmented >>= traverse (traverse rationalToIntMaybe)
  where
    rank = length rows
    identityRows :: [[Rational]]
    identityRows =
      [ [if i == j then 1 % 1 else 0 % 1 | j <- [0 .. rank - 1]]
      | i <- [0 .. rank - 1]
      ]
    augmented :: [[Rational]]
    augmented =
      zipWith (++) (map (map (\x -> toInteger x % 1)) rows) identityRows

    go i mat
      | i >= rank = do
          let (left, right) = unzip [splitAt rank row | row <- mat]
          if left == identityRows
            then pure right
            else Nothing
      | otherwise = do
          pivot <- findPivotRow i mat
          let swapped = swapRows i pivot mat
              pivotRow = swapped !! i
              pivotVal = pivotRow !! i
          if pivotVal == 0
            then Nothing
            else
              let normalized = replaceAt i (map (/ pivotVal) pivotRow) swapped
                  pivotRow' = normalized !! i
                  eliminated =
                    [ if rowIx == i
                        then row
                        else
                          let factor = row !! i
                           in zipWith (\x y -> x - factor * y) row pivotRow'
                    | (rowIx, row) <- zip [0 ..] normalized
                    ]
               in go (i + 1) eliminated

findPivotRow :: Int -> [[Rational]] -> Maybe Int
findPivotRow col rows =
  case [rowIx | (rowIx, row) <- zip [col ..] (drop col rows), row !! col /= 0] of
    pivot : _ -> Just pivot
    [] -> Nothing

swapRows :: Int -> Int -> [a] -> [a]
swapRows i j xs
  | i == j = xs
  | otherwise =
      [ pick k | k <- [0 .. length xs - 1] ]
  where
    xi = xs !! i
    xj = xs !! j
    pick k
      | k == i = xj
      | k == j = xi
      | otherwise = xs !! k

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs =
  take i xs ++ [x] ++ drop (i + 1) xs

rationalToIntMaybe :: Rational -> Maybe Int
rationalToIntMaybe q =
  if denominator q == 1
    then Just (fromInteger (numerator q))
    else Nothing

withParallelTeam :: Int -> (Team -> IO a) -> IO a
withParallelTeam workers = bracket (newParallelTeam workers) stopParallelTeam

-- | Module-level cache for a reusable parallel team.
--
-- OpenMP keeps a persistent global thread pool; this does the same. The MVar
-- serialises concurrent callers (two concurrent @parallel@ regions would
-- oversubscribe the hardware, so serialising is correct).
{-# NOINLINE globalParallelTeamVar #-}
globalParallelTeamVar :: MVar (Maybe Team)
globalParallelTeamVar = unsafePerformIO (newMVar Nothing)

-- | Acquire the global team (creating or re-creating it if needed), run an
-- action with it, then return it to the cache.
withGlobalTeam :: Int -> (Team -> IO a) -> IO a
withGlobalTeam workers action =
  modifyMVar globalParallelTeamVar $ \mTeam -> do
    team <- case mTeam of
      Just t | teamWorkerCount t == workers -> pure t
      Just t -> stopParallelTeam t >> newParallelTeam workers
      Nothing -> newParallelTeam workers
    result <- action team
    pure (Just team, result)

newParallelTeam :: Int -> IO Team
newParallelTeam workers = do
  let !count = max 1 workers
  handles <- mapM startWorker [1 .. count - 1]
  pure (Team count handles)

stopParallelTeam :: Team -> IO ()
stopParallelTeam (Team _ handles) = do
  mapM_ (\handle -> putMVar (workerCommand handle) StopWorker) handles
  mapM_ awaitWorkerResult handles

startWorker :: Int -> IO WorkerHandle
startWorker workerId = do
  command <- newEmptyMVar
  result <- newEmptyMVar
  _ <-
    forkIOWithUnmask $ \unmask ->
      let go = do
            cmd <- takeMVar command
            case cmd of
              StopWorker ->
                putMVar result (Right ())
              RunPhase phase -> do
                outcome <- try (unmask (runPhaseWorker phase workerId)) :: IO (Either SomeException ())
                putMVar result outcome
                go
              RunStaticWork start end action -> do
                outcome <- try (unmask (action workerId start end)) :: IO (Either SomeException ())
                putMVar result outcome
                go
       in go
  pure (WorkerHandle command result)

awaitWorkerResult :: WorkerHandle -> IO (Either SomeException ())
awaitWorkerResult handle = takeMVar (workerResult handle)

runParallelPhase :: Team -> Int -> Int -> (Int -> Int -> Int -> IO ()) -> IO ()
runParallelPhase team end chunkSize runChunk = do
  nextIndex <- newIORef 0
  failure <- newIORef Nothing
  let !phase =
        Phase
          { phaseEnd = end
          , phaseChunkSize = max 1 chunkSize
          , phaseNextIndex = nextIndex
          , phaseFailure = failure
          , phaseRunChunk = runChunk
          }
  mapM_ (\handle -> putMVar (workerCommand handle) (RunPhase phase)) (teamWorkers team)
  runPhaseWorker phase 0
  results <- mapM awaitWorkerResult (teamWorkers team)
  firstFailure <- readIORef failure
  case firstFailure of
    Just ex -> throwIO ex
    Nothing -> mapM_ (either throwIO pure) results

-- | Like 'runParallelPhase' but uses static pre-partitioned work assignment
-- instead of dynamic work stealing. Each worker receives exactly one
-- pre-computed slice [startFor w, startFor (w+1)) with no CAS operations.
--
-- Use for uniform-work loops (fills, maps, stencils) where load balance is
-- guaranteed. This mirrors OpenMP's @schedule(static)@.
runParallelPhaseStatic :: Team -> Int -> (Int -> Int -> Int -> IO ()) -> IO ()
runParallelPhaseStatic (Team workers handles) total runChunk
  | total <= 0 = pure ()
  | otherwise = do
      let !q = total `quot` workers
          !r = total `rem` workers
          -- First r workers get q+1 elements; the rest get q.
          startFor !w = w * q + min w r
      mapM_
        ( \(w, h) ->
            putMVar (workerCommand h) (RunStaticWork (startFor w) (startFor (w + 1)) runChunk)
        )
        (zip [1 ..] handles)
      -- Main thread (worker 0)
      let !s0 = startFor 0; !e0 = startFor 1
      mainResult <-
        if s0 < e0
          then try (runChunk 0 s0 e0) :: IO (Either SomeException ())
          else pure (Right ())
      workerResults <- mapM awaitWorkerResult handles
      case mainResult of
        Left ex -> throwIO ex
        Right () -> mapM_ (either throwIO pure) workerResults

runPhaseWorker :: Phase -> Int -> IO ()
runPhaseWorker phase workerId = go
  where
    go = do
      failed <- readIORef (phaseFailure phase)
      case failed of
        Just _ -> pure ()
        Nothing -> do
          start <- claimPhaseChunk phase
          if start >= phaseEnd phase
            then pure ()
            else do
              let !end = min (phaseEnd phase) (start + phaseChunkSize phase)
              result <- try (phaseRunChunk phase workerId start end) :: IO (Either SomeException ())
              case result of
                Right () -> go
                Left ex -> recordPhaseFailure phase ex

claimPhaseChunk :: Phase -> IO Int
claimPhaseChunk phase =
  atomicModifyIORef' (phaseNextIndex phase) $ \start ->
    let !next = start + phaseChunkSize phase
     in (next, start)

recordPhaseFailure :: Phase -> SomeException -> IO ()
recordPhaseFailure phase ex = do
  atomicModifyIORef' (phaseFailure phase) $ \existing ->
    case existing of
      Just prior -> (Just prior, ())
      Nothing -> (Just ex, ())
  atomicModifyIORef' (phaseNextIndex phase) (\_ -> (phaseEnd phase, ()))

chunkSizeFor :: Int -> Int -> Int
chunkSizeFor workers total =
  max 1 ((total + targetChunks - 1) `quot` targetChunks)
  where
    !targetChunks = max 1 (workers * 4)

{-# INLINE rectOfShape2 #-}
rectOfShape2 :: Sh2 -> Rect2
rectOfShape2 (Sh2 rows cols) = Rect2 0 0 rows cols

{-# INLINE rectOfShapeN #-}
rectOfShapeN :: ShN -> RectN
rectOfShapeN (ShN dims) = RectN (replicate (length dims) 0) dims

{-# INLINE transposeRect2 #-}
transposeRect2 :: Rect2 -> Rect2
transposeRect2 (Rect2 rowLo colLo rowHi colHi) =
  Rect2 colLo rowLo colHi rowHi

{-# INLINE isEmptyRect2 #-}
isEmptyRect2 :: Rect2 -> Bool
isEmptyRect2 (Rect2 rowLo colLo rowHi colHi) =
  rowHi <= rowLo || colHi <= colLo

{-# INLINE isEmptyRectN #-}
isEmptyRectN :: RectN -> Bool
isEmptyRectN (RectN lo hi) =
  length lo /= length hi || or (zipWith (>=) lo hi)

{-# INLINE inRect2 #-}
inRect2 :: Rect2 -> Ix2 -> Bool
inRect2 (Rect2 rowLo colLo rowHi colHi) (Ix2 row col) =
  row >= rowLo && row < rowHi && col >= colLo && col < colHi

{-# INLINE inRectN #-}
inRectN :: RectN -> IxN -> Bool
inRectN (RectN lo hi) (IxN is) =
  length lo == length hi
    && length lo == length is
    && and (zipWith3 (\lower upper i -> i >= lower && i < upper) lo hi is)

applyScheduleN :: ScheduleN -> RectN -> (IxN -> Prog ()) -> Prog ()
applyScheduleN (ScheduleN stages) =
  go stages
  where
    go [] rect emit =
      parForRectN rect emit
    go (ScheduleAffineStageN affine : rest) rect emit =
      applyAffineRectN affine rect (\rect' emit' -> go rest rect' emit') emit
    go (ScheduleTileStageN tileDims : rest) rect emit =
      tileRectN tileDims rect (\rect' -> go rest rect' emit)

{-# INLINE applyAffineRectN #-}
applyAffineRectN ::
  AffineN ->
  RectN ->
  (RectN -> (IxN -> Prog ()) -> Prog ()) ->
  (IxN -> Prog ()) ->
  Prog ()
applyAffineRectN affine rect k emit =
  case invertAffineN affine of
    Nothing ->
      invalidProgUsage "affine schedule stages must be invertible integer transforms"
    Just inverse ->
      let box = boundingBoxAffineN affine rect
          emit' ix =
            let source = applyAffineN inverse ix
             in if inRectN rect source
                  then emit source
                  else pure ()
       in k box emit'

{-# INLINE tileRectN #-}
tileRectN :: [Int] -> RectN -> (RectN -> Prog ()) -> Prog ()
tileRectN tileDims (RectN lo hi) body
  | length tileDims /= length lo || length lo /= length hi =
      invalidProgUsage "tileRectN requires tile extents and bounds with matching ranks"
  | any (<= 0) tileDims =
      invalidProgUsage "tileRectN requires positive tile extents"
  | otherwise =
      tileN tileDims (ShN extents) $ \(IxN offset) ->
        let start = zipWith (+) lo offset
         in body (RectN start (zipWith3 tileUpperBound hi start tileDims))
  where
    extents = zipWith (-) hi lo

{-# INLINE normalizeScheduleN #-}
normalizeScheduleN :: ScheduleN -> ScheduleN
normalizeScheduleN (ScheduleN stages) =
  ScheduleN (normalizeScheduleStagesN stages)

normalizeScheduleStagesN :: [ScheduleStageN] -> [ScheduleStageN]
normalizeScheduleStagesN stages =
  reverse (go stages Nothing [])
  where
    go [] pending acc =
      flushAffineStageN pending acc
    go (ScheduleAffineStageN affine : rest) pending acc =
      go rest (Just (maybe affine (composeAffineN affine) pending)) acc
    go (ScheduleTileStageN tileDims : rest) pending acc =
      go rest Nothing (ScheduleTileStageN tileDims : flushAffineStageN pending acc)

flushAffineStageN :: Maybe AffineN -> [ScheduleStageN] -> [ScheduleStageN]
flushAffineStageN Nothing acc = acc
flushAffineStageN (Just affine) acc
  | isIdentityAffineN affine = acc
  | otherwise = ScheduleAffineStageN affine : acc

{-# INLINE isIdentityAffineN #-}
isIdentityAffineN :: AffineN -> Bool
isIdentityAffineN (AffineN rows offset) =
  offset == replicate rank 0 && rows == identityRows
  where
    rank = length rows
    identityRows =
      [ [if i == j then 1 else 0 | j <- [0 .. rank - 1]]
      | i <- [0 .. rank - 1]
      ]

data StructuredAffine2D
  = StructuredIdentity
  | StructuredInterchange
  | StructuredSkew !Int
  | StructuredGeneric

{-# INLINE classifyAffine2 #-}
classifyAffine2 :: Affine2 -> StructuredAffine2D
classifyAffine2 (Affine2 1 0 0 1 0 0) = StructuredIdentity
classifyAffine2 (Affine2 0 1 1 0 0 0) = StructuredInterchange
classifyAffine2 (Affine2 1 0 factor 1 0 0) = StructuredSkew factor
classifyAffine2 _ = StructuredGeneric

{-# INLINE compileAffine2D #-}
compileAffine2D :: Affine2 -> CompiledAffine2D
compileAffine2D affine =
  case classifyAffine2 affine of
    StructuredIdentity -> CompiledIdentity2D
    StructuredInterchange -> CompiledInterchange2D
    StructuredSkew factor -> CompiledSkew2D factor
    StructuredGeneric -> CompiledGenericAffine2D affine

{-# INLINE domainRect2 #-}
domainRect2 :: Domain2D -> Rect2
domainRect2 (RectDomain rect) = rect
domainRect2 (SkewDomain rect _) = rect

{-# INLINE domainAffine2 #-}
domainAffine2 :: Domain2D -> Affine2
domainAffine2 (RectDomain _) = identityAffine2
domainAffine2 (SkewDomain _ factor) = skew2D factor

{-# INLINE skewDomain2D #-}
skewDomain2D :: Int -> Domain2D -> Domain2D
skewDomain2D factor (RectDomain rect) = SkewDomain rect factor
skewDomain2D factor (SkewDomain rect factor0) = SkewDomain rect (factor0 + factor)

{-# INLINE inDomain2D #-}
inDomain2D :: Domain2D -> Ix2 -> Bool
inDomain2D (RectDomain rect) ix = inRect2 rect ix
inDomain2D (SkewDomain rect factor) ix =
  inRect2 rect (applyAffine2 (Affine2 1 0 (-factor) 1 0 0) ix)

{-# INLINE boundingBoxAffineDomain2D #-}
boundingBoxAffineDomain2D :: Affine2 -> Domain2D -> Rect2
boundingBoxAffineDomain2D affine domain =
  boundingBoxAffine2D
    (composeAffine2 affine (domainAffine2 domain))
    (domainRect2 domain)

{-# INLINE parForDomain2DRepr #-}
parForDomain2DRepr ::
  Loop repr =>
  Domain2D ->
  (Ix2 -> repr ()) ->
  repr ()
parForDomain2DRepr (RectDomain rect) body =
  parForRect2DRepr rect body
parForDomain2DRepr (SkewDomain rect factor) body =
  parForRect2DRepr rect $ \ix ->
    withIx2 ix $ \row col ->
      body (Ix2 row (col + factor * row))

{-# INLINE parForRect2DRepr #-}
parForRect2DRepr ::
  Loop repr =>
  Rect2 ->
  (Ix2 -> repr ()) ->
  repr ()
parForRect2DRepr (Rect2 rowLo colLo rowHi colHi) body =
  case rowLo of
    I# rowLo# ->
      case colLo of
        I# colLo# ->
          case rowHi - rowLo of
            I# rowCount# ->
              case colHi - colLo of
                I# colCount# ->
                      loopParFor2#
                        rowCount#
                        colCount#
                        (\i# j# -> body (Ix2 (I# (rowLo# +# i#)) (I# (colLo# +# j#))))

{-# INLINE parForWavefrontRect2DRepr #-}
parForWavefrontRect2DRepr ::
  Loop repr =>
  Rect2 ->
  (Ix2 -> repr ()) ->
  repr ()
parForWavefrontRect2DRepr (Rect2 rowLo colLo rowHi colHi) body
  | rows <= 0 || cols <= 0 = pure ()
  | otherwise = go 0
  where
    !rows = rowHi - rowLo
    !cols = colHi - colLo
    !maxDiag = rows + cols - 1

    go !diag
      | diag >= maxDiag = pure ()
      | otherwise =
          let !rowStart = max 0 (diag - (cols - 1))
              !rowEnd = min (rows - 1) diag
              !count = rowEnd - rowStart + 1
           in if count <= 0
                then go (diag + 1)
                else do
                  loopParFor count $ \offset ->
                    let !row = rowLo + rowStart + offset
                        !col = colLo + diag - (rowStart + offset)
                     in body (Ix2 row col)
                  go (diag + 1)

{-# INLINE applyAffineRect2DRepr #-}
applyAffineRect2DRepr ::
  Loop repr =>
  Affine2 ->
  Rect2 ->
  (Rect2 -> (Ix2 -> repr ()) -> repr ()) ->
  (Ix2 -> repr ()) ->
  repr ()
applyAffineRect2DRepr affine rect k emit =
  applyAffineDomain2DRepr
    affine
    (RectDomain rect)
    (\domain emit' -> k (domainRect2 domain) emit')
    emit

{-# INLINE applyAffineDomain2DRepr #-}
applyAffineDomain2DRepr ::
  Loop repr =>
  Affine2 ->
  Domain2D ->
  (Domain2D -> (Ix2 -> repr ()) -> repr ()) ->
  (Ix2 -> repr ()) ->
  repr ()
applyAffineDomain2DRepr affine domain k emit =
  case invertAffine2 affine of
    Nothing ->
      invalidUsageRepr "affine loop transformations require an invertible integer transform"
    Just inverse ->
      let !box = boundingBoxAffineDomain2D affine domain
          emit' ix' =
            let !ix = applyAffine2 inverse ix'
             in case inDomain2D domain ix of
                  True -> emit ix
                  False -> pure ()
       in k (RectDomain box) emit'

{-# INLINE tileRect2DRepr #-}
tileRect2DRepr ::
  Loop repr =>
  Int ->
  Int ->
  Rect2 ->
  (Rect2 -> repr ()) ->
  repr ()
tileRect2DRepr tileRows tileCols (Rect2 rowLo colLo rowHi colHi) body =
  case tileRows of
    I# tileRows# ->
      case tileCols of
        I# tileCols# ->
          case rowLo of
            I# rowLo# ->
              case colLo of
                I# colLo# ->
                  case rowHi - rowLo of
                    I# rows# ->
                      case colHi - colLo of
                        I# cols# ->
                          case tileRows# <=# 0# of
                            1# -> invalidUsageRepr "tileRect2D requires a positive row tile size"
                            _ ->
                              case tileCols# <=# 0# of
                                1# -> invalidUsageRepr "tileRect2D requires a positive column tile size"
                                _ ->
                                  case rows# <=# 0# of
                                    1# -> pure ()
                                    _ ->
                                      case cols# <=# 0# of
                                        1# -> pure ()
                                        _ ->
                                          loopParFor2#
                                            (tileCount# rows# tileRows#)
                                            (tileCount# cols# tileCols#)
                                            (\tileI# tileJ# ->
                                               let !row0# = rowLo# +# (tileI# *# tileRows#)
                                                   !col0# = colLo# +# (tileJ# *# tileCols#)
                                                   !rowCount# = tileSpan# rows# (tileI# *# tileRows#) tileRows#
                                                   !colCount# = tileSpan# cols# (tileJ# *# tileCols#) tileCols#
                                                in body
                                                     (Rect2
                                                         (I# row0#)
                                                         (I# col0#)
                                                         (I# (row0# +# rowCount#))
                                                         (I# (col0# +# colCount#))))

{-# INLINE tileDomain2DRepr #-}
tileDomain2DRepr ::
  Loop repr =>
  Int ->
  Int ->
  Domain2D ->
  (Domain2D -> repr ()) ->
  repr ()
tileDomain2DRepr tileRows tileCols (RectDomain rect) body =
  tileRect2DRepr tileRows tileCols rect (\tileRect -> body (RectDomain tileRect))
tileDomain2DRepr tileRows tileCols (SkewDomain rect factor) body =
  tileRect2DRepr tileRows tileCols rect (\tileRect -> body (SkewDomain tileRect factor))
