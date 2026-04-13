{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Loom.Internal.Kernel
  ( Arr
  , Prog
  , Reducer
  , RedVar
  , newArr
  , fromList
  , toList
  , sizeOfArr
  , readArrIO
  , writeArrIO
  , runProg
  , parFor
  , readArr
  , writeArr
  , newReducer
  , reduce
  , getReducer
  , foldFor
  , mkReducer
  , intSum
  , doubleSum
  ) where

import Control.Concurrent (forkIOWithUnmask, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try, throwIO)
import Control.Monad (ap)
import Control.Monad.ST (RealWorld)
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

data ReducerSpec rep a = ReducerSpec
  { reducerInit :: !rep
  , reducerStep :: rep -> a -> rep
  , reducerDone :: rep -> a
  }

data Reducer a where
  Reducer :: Prim rep => !(ReducerSpec rep a) -> Reducer a

data RedVar a where
  RedVar :: Prim rep => !(PrimVar RealWorld rep) -> !(ReducerSpec rep a) -> RedVar a

class Monad repr => Loop repr where
  loopParFor :: Int -> (Int -> repr ()) -> repr ()
  loopReadArr :: Prim a => Arr a -> Int -> repr a
  loopWriteArr :: Prim a => Arr a -> Int -> a -> repr ()
  loopNewReducer :: Reducer a -> (RedVar a -> repr r) -> repr r
  loopReduce :: RedVar a -> a -> repr ()
  loopGetReducer :: RedVar a -> repr a
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

newtype Kernel a = Kernel
  { runKernel :: Int -> IO a
  }

instance Functor Kernel where
  fmap f (Kernel action) = Kernel (\depth -> fmap f (action depth))

instance Applicative Kernel where
  pure x = Kernel (\_ -> pure x)
  Kernel mf <*> Kernel mx = Kernel (\depth -> mf depth <*> mx depth)

instance Monad Kernel where
  Kernel action >>= f = Kernel $ \depth -> do
    x <- action depth
    runKernel (f x) depth

instance Loop Kernel where
  loopParFor n body = Kernel go
    where
      go !depth = do
        caps <- getNumCapabilities
        let workers = min caps n
            childDepth = depth + 1
        if depth == 0 && workers > 1
          then runParallel childDepth workers
          else runSequential childDepth 0

      runSequential !childDepth = goSeq
        where
          goSeq !i
            | i >= n = pure ()
            | otherwise = runKernel (body i) childDepth >> goSeq (i + 1)

      runParallel !childDepth !workers = do
        let chunks = chunkRanges workers n
        vars <- mapM (forkChunk childDepth) chunks
        mapM_ awaitChunk vars

      forkChunk !childDepth (!start, !end) = do
        var <- newEmptyMVar
        _ <-
          forkIOWithUnmask $ \unmask -> do
            result <- try (unmask (runChunk childDepth start end)) :: IO (Either SomeException ())
            putMVar var result
        pure var

      runChunk !childDepth !start !end = goChunk start
        where
          goChunk !i
            | i >= end = pure ()
            | otherwise = runKernel (body i) childDepth >> goChunk (i + 1)

      awaitChunk var = do
        result <- takeMVar var
        either throwIO pure result

  loopReadArr (Arr _ arr) i = Kernel (\_ -> readPrimArray arr i)

  loopWriteArr (Arr _ arr) i x = Kernel (\_ -> writePrimArray arr i x)

  loopNewReducer reducer body =
    Kernel $ \depth -> do
      redVar <- newRedVar reducer
      runKernel (body redVar) depth

  loopReduce (RedVar var spec) x =
    Kernel $ \_ -> do
      !acc <- readPrimVar var
      let !acc' = reducerStep spec acc x
      writePrimVar var acc'

  loopGetReducer (RedVar var spec) = Kernel $ \_ -> do
    !acc <- readPrimVar var
    pure (reducerDone spec acc)

  loopFoldFor (Reducer spec) n body = Kernel $ \depth ->
    go depth 0 (reducerInit spec)
    where
      go !depth !i !acc
        | i >= n = pure (reducerDone spec acc)
        | otherwise = do
            x <- runKernel (body i) depth
            let !acc' = reducerStep spec acc x
            go depth (i + 1) acc'

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
runProg (Prog m) = runKernel (m pure) 0

{-# INLINE parFor #-}
parFor :: Int -> (Int -> Prog ()) -> Prog ()
parFor n body = Prog $ \k -> do
  loopParFor n (\i -> unProg (body i) (\() -> pure ()))
  k ()

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

{-# INLINE foldFor #-}
foldFor :: Reducer a -> Int -> (Int -> Prog a) -> Prog a
foldFor reducer n body =
  Prog $ \k -> loopFoldFor reducer n (\i -> unProg (body i) pure) >>= k

{-# INLINE mkReducer #-}
mkReducer :: Prim rep => rep -> (rep -> a -> rep) -> (rep -> a) -> Reducer a
mkReducer initial step done = Reducer (ReducerSpec initial step done)

{-# INLINE intSum #-}
intSum :: Reducer Int
intSum = mkReducer 0 step id
  where
    step !acc !x = acc + x

{-# INLINE doubleSum #-}
doubleSum :: Reducer Double
doubleSum = mkReducer 0 step id
  where
    step !acc !x = acc + x

newRedVar :: Reducer a -> IO (RedVar a)
newRedVar (Reducer spec) = do
  var <- newPrimVar (reducerInit spec)
  pure (RedVar var spec)

chunkRanges :: Int -> Int -> [(Int, Int)]
chunkRanges !workers !n = go 0
  where
    !chunkSize = max 1 ((n + workers - 1) `quot` workers)
    go !start
      | start >= n = []
      | otherwise =
          let !end = min n (start + chunkSize)
           in (start, end) : go end
