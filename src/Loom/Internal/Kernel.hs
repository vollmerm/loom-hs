{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Loom.Internal.Kernel
  ( Arr
  , Ix1
  , Ix2
  , Sh1
  , Sh2
  , Prog
  , Reducer
  , RedVar
  , AccVar
  , ix1
  , ix2
  , sh1
  , sh2
  , newArr
  , fromList
  , toList
  , unIx1
  , unIx2
  , withIx2
  , index1
  , index2
  , sizeOfArr
  , readArrIO
  , writeArrIO
  , runProg
  , parallel
  , barrier
  , parFor
  , parForSh1
  , parForSh2
  , parFor2
  , readArr
  , writeArr
  , newReducer
  , reduce
  , getReducer
  , accumFor
  , newAcc
  , readAcc
  , writeAcc
  , foldFor
  , mkReducer
  , mkReducerWith
  , intSum
  , doubleSum
  ) where

import Control.Concurrent (MVar, forkIOWithUnmask, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (ap)
import Control.Monad.ST (RealWorld)
import Data.Primitive.Array
  ( MutableArray
  , newArray
  , readArray
  , writeArray
  )
import Data.Primitive.PrimArray
  ( MutablePrimArray
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
import GHC.Conc (getNumCapabilities)

data Arr a = Arr !Int !(MutablePrimArray RealWorld a)

newtype Ix1 = Ix1 Int

data Ix2 = Ix2 {-# UNPACK #-} !Int {-# UNPACK #-} !Int

newtype Sh1 = Sh1 Int

data Sh2 = Sh2 {-# UNPACK #-} !Int {-# UNPACK #-} !Int

data ReducerSpec rep a = ReducerSpec
  { reducerInit :: !rep
  , reducerStep :: rep -> a -> rep
  , reducerMerge :: rep -> rep -> rep
  , reducerDone :: rep -> a
  }

data Reducer a where
  Reducer :: Prim rep => !(ReducerSpec rep a) -> Reducer a

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

data AccVar a where
  AccVar :: Prim a => !(PrimVar RealWorld a) -> AccVar a

class Monad repr => Loop repr where
  loopParallel :: repr a -> repr a
  loopBarrier :: repr ()
  loopParFor :: Int -> (Int -> repr ()) -> repr ()
  loopParFor2 :: Int -> Int -> (Int -> Int -> repr ()) -> repr ()
  loopReadArr :: Prim a => Arr a -> Int -> repr a
  loopWriteArr :: Prim a => Arr a -> Int -> a -> repr ()
  loopNewReducer :: Reducer a -> (RedVar a -> repr r) -> repr r
  loopReduce :: RedVar a -> a -> repr ()
  loopGetReducer :: RedVar a -> repr a
  loopAccumFor :: Int -> a -> (a -> Int -> repr a) -> repr a
  loopNewAcc :: Prim a => a -> (AccVar a -> repr r) -> repr r
  loopReadAcc :: Prim a => AccVar a -> repr a
  loopWriteAcc :: Prim a => AccVar a -> a -> repr ()
  loopFoldFor :: Reducer a -> Int -> (Int -> repr a) -> repr a

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
  { rtParallelWorkers :: !(Maybe Int)
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
    case (rtParallelWorkers rt, rtWorkerId rt, rtLoopDepth rt) of
      (Nothing, Nothing, 0) -> do
        caps <- getNumCapabilities
        let !workers = max 1 caps
        runKernel body (Runtime (Just workers) Nothing 0)
      _ ->
        invalidUsage "parallel regions cannot be nested or entered from inside a loop body"

  loopBarrier = Kernel $ \rt ->
    case (rtParallelWorkers rt, rtWorkerId rt, rtLoopDepth rt) of
      (Nothing, _, _) ->
        invalidUsage "barrier may only be used inside a parallel region"
      (_, Just _, _) ->
        invalidUsage "barrier may not appear inside a parallel loop body"
      (_, _, depth)
        | depth > 0 ->
            invalidUsage "barrier may only appear between parallel phases"
      _ ->
        pure ()

  loopParFor n body = Kernel $ \rt -> do
    workers0 <-
      case rtParallelWorkers rt of
        Just workers -> pure workers
        Nothing -> getNumCapabilities
    let !workers = min n (max 1 workers0)
        childRt = rt {rtLoopDepth = rtLoopDepth rt + 1}
        canRunParallel =
          rtLoopDepth rt == 0
            && rtWorkerId rt == Nothing
            && workers > 1
    if canRunParallel
      then runParallel workers childRt
      else runSequential childRt 0
    where
      runSequential !childRt = goSeq
        where
          goSeq !i
            | i >= n = pure ()
            | otherwise = runKernel (body i) childRt >> goSeq (i + 1)

      runParallel !workers !childRt = do
        let chunks = chunkRanges workers n
        vars <- mapM (forkChunk childRt) (zip [0 ..] chunks)
        mapM_ awaitChunkResult vars

      forkChunk !childRt (!workerId, (!start, !end)) = do
        var <- newEmptyMVar
        _ <-
          forkIOWithUnmask $ \unmask -> do
            let workerRt = childRt {rtWorkerId = Just workerId}
            result <- try (unmask (runChunk workerRt start end)) :: IO (Either SomeException ())
            putMVar var result
        pure var

      runChunk !workerRt !start !end = goChunk start
        where
          goChunk !i
            | i >= end = pure ()
            | otherwise = runKernel (body i) workerRt >> goChunk (i + 1)

  loopParFor2 n m body = Kernel $ \rt ->
    if n <= 0 || m <= 0
      then pure ()
      else do
        let !total = n * m
        workers0 <-
          case rtParallelWorkers rt of
            Just workers -> pure workers
            Nothing -> getNumCapabilities
        let !workers = min total (max 1 workers0)
            childRt = rt {rtLoopDepth = rtLoopDepth rt + 1}
            canRunParallel =
              rtLoopDepth rt == 0
                && rtWorkerId rt == Nothing
                && workers > 1
        if canRunParallel
          then runParallel2 total workers childRt
          else runSequential2 childRt 0 total
    where
      runSequential2 !childRt !start !end
        | start >= end = pure ()
        | otherwise =
            let !i0 = start `quot` m
                !j0 = start - i0 * m
             in go2 childRt start end i0 j0

      go2 !childRt !idx !end !i !j
        | idx >= end = pure ()
        | otherwise = do
            runKernel (body i j) childRt
            let !j' = j + 1
            if j' < m
              then go2 childRt (idx + 1) end i j'
              else go2 childRt (idx + 1) end (i + 1) 0

      runParallel2 !total !workers !childRt = do
        let chunks = chunkRanges workers total
        vars <- mapM (forkChunk2 childRt) (zip [0 ..] chunks)
        mapM_ awaitChunkResult vars

      forkChunk2 !childRt (!workerId, (!start, !end)) = do
        var <- newEmptyMVar
        _ <-
          forkIOWithUnmask $ \unmask -> do
            let workerRt = childRt {rtWorkerId = Just workerId}
            result <- try (unmask (runSequential2 workerRt start end)) :: IO (Either SomeException ())
            putMVar var result
        pure var

  loopReadArr (Arr _ arr) i = Kernel (\_ -> readPrimArray arr i)

  loopWriteArr (Arr _ arr) i x = Kernel (\_ -> writePrimArray arr i x)

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

  loopNewAcc initial body =
    Kernel $ \rt -> do
      var <- newPrimVar initial
      runKernel (body (AccVar var)) rt

  loopReadAcc (AccVar var) = Kernel (\_ -> readPrimVar var)

  loopWriteAcc (AccVar var) x = Kernel (\_ -> writePrimVar var x)

  loopFoldFor (Reducer spec) n body = Kernel $ \rt ->
    let go !i !acc
          | i >= n = pure (reducerDone spec acc)
          | otherwise = do
              x <- runKernel (body i) rt
              let !acc' = reducerStep spec acc x
              go (i + 1) acc'
     in go 0 (reducerInit spec)

{-# INLINE newArr #-}
newArr :: Prim a => Int -> IO (Arr a)
newArr n = Arr n <$> newPrimArray n

{-# INLINE sizeOfArr #-}
sizeOfArr :: Prim a => Arr a -> Int
sizeOfArr (Arr n _) = n

{-# INLINE readArrIO #-}
readArrIO :: Prim a => Arr a -> Int -> IO a
readArrIO (Arr _ arr) i = readPrimArray arr i

{-# INLINE writeArrIO #-}
writeArrIO :: Prim a => Arr a -> Int -> a -> IO ()
writeArrIO (Arr _ arr) i x = writePrimArray arr i x

fromList :: Prim a => [a] -> IO (Arr a)
fromList xs = do
  let !n = length xs
  arr <- newArr n
  let go !_ [] = pure arr
      go !i (y : ys) = writeArrIO arr i y >> go (i + 1) ys
  go 0 xs

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
runProg :: Prog a -> IO a
runProg (Prog m) = runKernel (m pure) (Runtime Nothing Nothing 0)

{-# INLINE ix1 #-}
ix1 :: Int -> Ix1
ix1 = Ix1

{-# INLINE ix2 #-}
ix2 :: Int -> Int -> Ix2
ix2 = Ix2

{-# INLINE sh1 #-}
sh1 :: Int -> Sh1
sh1 = Sh1

{-# INLINE sh2 #-}
sh2 :: Int -> Int -> Sh2
sh2 = Sh2

{-# INLINE unIx1 #-}
unIx1 :: Ix1 -> Int
unIx1 (Ix1 i) = i

{-# INLINE unIx2 #-}
unIx2 :: Ix2 -> (Int, Int)
unIx2 (Ix2 i j) = (i, j)

{-# INLINE withIx2 #-}
withIx2 :: Ix2 -> (Int -> Int -> r) -> r
withIx2 (Ix2 i j) f = f i j

{-# INLINE index1 #-}
index1 :: Sh1 -> Ix1 -> Int
index1 (Sh1 _) (Ix1 i) = i

{-# INLINE index2 #-}
index2 :: Sh2 -> Ix2 -> Int
index2 (Sh2 _ n) (Ix2 i j) = i * n + j

{-# INLINE parallel #-}
parallel :: Prog a -> Prog a
parallel body = Prog $ \k -> loopParallel (unProg body k)

{-# INLINE barrier #-}
barrier :: Prog ()
barrier = Prog $ \k -> do
  loopBarrier
  k ()

{-# INLINE parFor #-}
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
parForSh2 (Sh2 n m) body = Prog $ \k -> do
  loopParFor2 n m (\i j -> unProg (body (Ix2 i j)) (\() -> pure ()))
  k ()

{-# INLINE parFor2 #-}
parFor2 :: Int -> Int -> (Int -> Int -> Prog ()) -> Prog ()
parFor2 n m body =
  parForSh2 (Sh2 n m) $ \ix ->
    withIx2 ix body

{-# INLINE readArr #-}
readArr :: Prim a => Arr a -> Int -> Prog a
readArr arr i = Prog $ \k -> loopReadArr arr i >>= k

{-# INLINE writeArr #-}
writeArr :: Prim a => Arr a -> Int -> a -> Prog ()
writeArr arr i x = Prog $ \k -> do
  loopWriteArr arr i x
  k ()

{-# INLINE newReducer #-}
newReducer :: Reducer a -> (RedVar a -> Prog r) -> Prog r
newReducer reducer body = Prog $ \k ->
  loopNewReducer reducer (\redVar -> unProg (body redVar) k)

{-# INLINE reduce #-}
reduce :: RedVar a -> a -> Prog ()
reduce redVar x = Prog $ \k -> do
  loopReduce redVar x
  k ()

{-# INLINE getReducer #-}
getReducer :: RedVar a -> Prog a
getReducer redVar = Prog $ \k -> loopGetReducer redVar >>= k

{-# INLINE accumFor #-}
accumFor :: Int -> a -> (a -> Int -> Prog a) -> Prog a
accumFor n initial body =
  Prog $ \k -> loopAccumFor n initial (\acc i -> unProg (body acc i) pure) >>= k

{-# INLINE newAcc #-}
newAcc :: Prim a => a -> (AccVar a -> Prog r) -> Prog r
newAcc initial body = Prog $ \k ->
  loopNewAcc initial (\accVar -> unProg (body accVar) k)

{-# INLINE readAcc #-}
readAcc :: Prim a => AccVar a -> Prog a
readAcc accVar = Prog $ \k -> loopReadAcc accVar >>= k

{-# INLINE writeAcc #-}
writeAcc :: Prim a => AccVar a -> a -> Prog ()
writeAcc accVar x = Prog $ \k -> do
  loopWriteAcc accVar x
  k ()

{-# INLINE foldFor #-}
foldFor :: Reducer a -> Int -> (Int -> Prog a) -> Prog a
foldFor reducer n body =
  Prog $ \k -> loopFoldFor reducer n (\i -> unProg (body i) pure) >>= k

{-# INLINE mkReducer #-}
mkReducer :: Prim rep => rep -> (rep -> a -> rep) -> (rep -> a) -> Reducer a
mkReducer initial step done = mkReducerWith initial step merge done
  where
    merge !left !right = step left (done right)

{-# INLINE mkReducerWith #-}
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
intSum :: Reducer Int
intSum = mkReducerWith 0 step merge id
  where
    step !acc !x = acc + x
    merge !left !right = left + right

{-# INLINE doubleSum #-}
doubleSum :: Reducer Double
doubleSum = mkReducerWith 0 step merge id
  where
    step !acc !x = acc + x
    merge !left !right = left + right

shouldShareReducer :: Runtime -> Bool
shouldShareReducer rt =
  case (rtParallelWorkers rt, rtWorkerId rt, rtLoopDepth rt) of
    (Just _, Nothing, 0) -> True
    _ -> False

parallelWorkers :: Runtime -> Int
parallelWorkers rt =
  case rtParallelWorkers rt of
    Just workers -> workers
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

awaitChunkResult :: MVar (Either SomeException ()) -> IO ()
awaitChunkResult var = do
  result <- takeMVar var
  either throwIO pure result

chunkRanges :: Int -> Int -> [(Int, Int)]
chunkRanges !workers !n = go 0
  where
    !chunkSize = max 1 ((n + workers - 1) `quot` workers)
    go !start
      | start >= n = []
      | otherwise =
          let !end = min n (start + chunkSize)
           in (start, end) : go end
