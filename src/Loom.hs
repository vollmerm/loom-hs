{-|
Module      : Loom
Description : Shape-first parallel loops for array-oriented kernels.
Copyright   :
License     : BSD-3-Clause

Loom is the main entry point for writing parallel kernels with a small,
shape-first API.

The usual workflow is:

1. allocate or wrap arrays,
2. describe the iteration space with 'shape' or one of the rank-specific loop
   combinators,
3. optionally attach a schedule with
   <https://hackage.haskell.org/package/loom-hs/docs/Loom.html#v:withSchedule 'withSchedule'>,
4. read and write array elements inside 'parallel' code,
5. run the kernel with 'runProg'.

For most code, import the Loom module together with qualified schedule constructors:

@
import Loom
import qualified Loom.Schedule as Schedule
@

A small 2D example:

@
step :: Int -> Int -> Arr Double -> Arr Double -> IO ()
step rows cols input output =
  runProg $ parallel $
    for2 rows cols (Schedule.tile2 32 32) $ \\i j -> do
      let ix = i * cols + j
      x <- readArr input ix
      writeArr output ix (x + 1)
@

Use the Loom.Schedule module to build schedules, Loom.Polyhedral for legality analysis
and lowering, and Loom.Expert only when you need the low-level loop and shape
representations directly.
-}
module Loom
  (
    -- * Core types
    Arr
  , Index
  , Domain
  , Schedule
  , Vec
  , IVec
  , I32Vec
  , DVec
  , Prog
  , Reducer
  , RedVar
  , AccVar

    -- * Shape-first loop API
  , shape
  , withSchedule
  , forEach
  , for1
  , for2
  , for3
  , coords
  , linearIndex

    -- * Arrays and execution
  , newArr
  , fromList
  , toList
  , vecWidth
  , sizeOfArr
  , readArrIO
  , writeArrIO
  , runProg
  , parallel
  , barrier
  , stripMine

    -- * SIMD array operations
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
  , parFoldFor
  , mkReducer
  , mkReducerWith
  , intSum
  , int32Sum
  , doubleSum
  ) where

import Loom.Internal.Kernel
  ( AccVar
  , Arr
  , DVec
  , I32Vec
  , IVec
  , IxN
  , Prog
  , RedVar
  , Reducer
  , Vec
  , accumDVecFor
  , accumFor
  , accumI32VecFor
  , accumIVecFor
  , accumVecFor
  , addDVec
  , addI32Vec
  , addIVec
  , addVec
  , barrier
  , broadcastDVec
  , broadcastI32Vec
  , broadcastIVec
  , broadcastVec
  , doubleSum
  , foldFor
  , parFoldFor
  , fromList
  , getReducer
  , int32Sum
  , intSum
  , indexN
  , ixN
  , newAcc
  , newArr
  , newReducer
  , parFor
  , parFor2
  , parFor3
  , parForScheduleN
  , parForShN
  , parForTransform2D
  , parallel
  , readAcc
  , readArr
  , readArrIO
  , readDVec
  , readI32Vec
  , readIVec
  , readVec
  , reduce
  , runProg
  , shN
  , sh2
  , sh3
  , sizeOfArr
  , stripMine
  , sumDVec
  , sumI32Vec
  , sumIVec
  , sumVec
  , toList
  , unIxN
  , withIx2
  , vecWidth
  , writeAcc
  , writeArr
  , writeArrIO
  , writeDVec
  , writeI32Vec
  , writeIVec
  , writeVec
  , mkReducer
  , mkReducerWith
  , mulDVec
  , mulI32Vec
  , mulIVec
  , mulVec
  , tiledFor3D
  )
import qualified Loom.Internal.Schedule as InternalSchedule
import Loom.Schedule (Schedule)

-- | An N-dimensional index value passed to 'forEach'.
--
-- Use 'coords' to inspect the coordinates directly, or prefer 'for1', 'for2',
-- and 'for3' when you want fixed-rank loop variables.
type Index = IxN

-- | An iteration domain built from extents and, optionally, a schedule.
--
-- Construct domains with 'shape' and attach schedules with 'withSchedule'.
-- Most code can avoid handling domain values directly by using 'for1',
-- 'for2', or 'for3'.
data Domain = Domain ![Int] !(Maybe Schedule)

-- | Build an unscheduled iteration domain from a list of extents.
--
-- Each element of the list is the extent of one loop dimension.
--
-- @
-- shape [rows, cols]
-- shape [depth, rows, cols]
-- @
shape :: [Int] -> Domain
shape dims = Domain dims Nothing
{-# INLINE shape #-}

infixl 5 `withSchedule`

-- | Attach a schedule to a domain.
--
-- Repeated applications compose left to right, so:
--
-- @
-- shape dims `withSchedule` s1 `withSchedule` s2
-- @
--
-- applies @s1@ and then @s2@.
withSchedule :: Domain -> Schedule -> Domain
withSchedule (Domain dims maybeSchedule) schedule =
  Domain dims (Just combined)
  where
    combined =
      case maybeSchedule of
        Nothing -> schedule
        Just current -> InternalSchedule.compose current schedule
{-# INLINE withSchedule #-}

-- | Traverse an iteration domain in parallel.
--
-- 'forEach' is the rank-polymorphic loop combinator. It is a good fit when the
-- rank is not known until runtime or when a kernel naturally works with an
-- index value as a whole.
--
-- When the rank is fixed in the source code, prefer 'for1', 'for2', or 'for3'
-- for clearer loop bodies.
--
-- @
-- runProg $ parallel $
--   forEach (shape [rows, cols] `withSchedule` Schedule.tile [32, 32]) $ \\ix -> do
--     let [i, j] = coords ix
--     ...
-- @
forEach :: Domain -> (Index -> Prog ()) -> Prog ()
forEach (Domain dims maybeSchedule) body =
  case (dims, maybeSchedule) of
    ([n], Nothing) ->
      parFor n (\i -> body (index1 i))
    ([rows, cols], Nothing) ->
      parFor2 rows cols (\i j -> body (index2 i j))
    ([depth, rows, cols], Nothing) ->
      parFor3 depth rows cols (\i j k -> body (index3 i j k))
    ([rows, cols], Just schedule) ->
      case InternalSchedule.compileSchedule2D schedule of
        Just transform ->
          parForTransform2D transform (sh2 rows cols) (\ix -> withIx2 ix (\i j -> body (index2 i j)))
        Nothing ->
          parForScheduleN (InternalSchedule.lowerScheduleN schedule) (shN dims) body
    ([depth, rows, cols], Just schedule) ->
      case InternalSchedule.tileSchedule3D schedule of
        Just (tileDepth, tileRows, tileCols) ->
          tiledFor3D tileDepth tileRows tileCols (sh3 depth rows cols) (\i j k -> body (index3 i j k))
        Nothing ->
          parForScheduleN (InternalSchedule.lowerScheduleN schedule) (shN dims) body
    (_, Just schedule) ->
      parForScheduleN (InternalSchedule.lowerScheduleN schedule) (shN dims) body
    _ ->
      parForShN (shN dims) body
{-# INLINE forEach #-}

-- | Run a one-dimensional parallel loop with an explicit schedule.
--
-- Use @Schedule.identity@ when you want a plain parallel loop.
--
-- @
-- for1 n Schedule.identity $ \\i -> ...
-- @
for1 :: Int -> Schedule -> (Int -> Prog ()) -> Prog ()
for1 n schedule body =
  if InternalSchedule.isIdentitySchedule schedule
    then parFor n body
    else forScheduled [n] schedule $ \ix ->
      withIndex1 "for1" ix body
{-# INLINE for1 #-}

-- | Run a two-dimensional parallel loop with an explicit schedule.
--
-- This is the most common entry point for matrix-shaped kernels.
--
-- @
-- for2 rows cols (Schedule.tile2 32 32) $ \\i j -> ...
-- @
for2 :: Int -> Int -> Schedule -> (Int -> Int -> Prog ()) -> Prog ()
for2 rows cols schedule body =
  if InternalSchedule.isIdentitySchedule schedule
    then parFor2 rows cols body
    else
      case InternalSchedule.compileSchedule2D schedule of
        Just transform ->
          parForTransform2D transform (sh2 rows cols) (\ix -> withIx2 ix body)
        Nothing ->
          forScheduled [rows, cols] schedule $ \ix ->
            withIndex2 "for2" ix body
{-# INLINE for2 #-}

-- | Run a three-dimensional parallel loop with an explicit schedule.
--
-- @
-- for3 depth rows cols (Schedule.tile3 8 8 8) $ \\d i j -> ...
-- @
for3 :: Int -> Int -> Int -> Schedule -> (Int -> Int -> Int -> Prog ()) -> Prog ()
for3 depth rows cols schedule body =
  if InternalSchedule.isIdentitySchedule schedule
    then parFor3 depth rows cols body
    else
      case InternalSchedule.tileSchedule3D schedule of
        Just (tileDepth, tileRows, tileCols) ->
          tiledFor3D tileDepth tileRows tileCols (sh3 depth rows cols) body
        Nothing ->
          forScheduled [depth, rows, cols] schedule $ \ix ->
            withIndex3 "for3" ix body
{-# INLINE for3 #-}

-- | Return the coordinates stored in an 'Index'.
--
-- This is most useful with 'forEach'. Fixed-rank loops usually read more
-- clearly with 'for1', 'for2', or 'for3'.
coords :: Index -> [Int]
coords = unIxN
{-# INLINE coords #-}

-- | Convert an 'Index' into a row-major linear offset for the given extents.
--
-- The list of extents should match the shape of the index.
--
-- @
-- forEach (shape [rows, cols]) $ \ix -> do
--   let offset = linearIndex [rows, cols] ix
--   ...
-- @
linearIndex :: [Int] -> Index -> Int
linearIndex dims = indexN (shN dims)
{-# INLINE linearIndex #-}

forScheduled :: [Int] -> Schedule -> (Index -> Prog ()) -> Prog ()
forScheduled dims schedule =
  forEach (shape dims `withSchedule` schedule)
{-# INLINE forScheduled #-}

withIndex1 :: String -> Index -> (Int -> Prog ()) -> Prog ()
withIndex1 caller ix body =
  case coords ix of
    [i] -> body i
    coords' -> badIndex caller 1 coords'
{-# INLINE withIndex1 #-}

withIndex2 :: String -> Index -> (Int -> Int -> Prog ()) -> Prog ()
withIndex2 caller ix body =
  case coords ix of
    [i, j] -> body i j
    coords' -> badIndex caller 2 coords'
{-# INLINE withIndex2 #-}

withIndex3 :: String -> Index -> (Int -> Int -> Int -> Prog ()) -> Prog ()
withIndex3 caller ix body =
  case coords ix of
    [i, j, k] -> body i j k
    coords' -> badIndex caller 3 coords'
{-# INLINE withIndex3 #-}

badIndex :: String -> Int -> [Int] -> Prog a
badIndex caller rank coords' =
  error
    ( "Loom."
        ++ caller
        ++ ": expected a "
        ++ show rank
        ++ "D index, got "
        ++ show coords'
    )
{-# INLINE badIndex #-}

index1 :: Int -> Index
index1 i = ixN [i]
{-# INLINE index1 #-}

index2 :: Int -> Int -> Index
index2 i j = ixN [i, j]
{-# INLINE index2 #-}

index3 :: Int -> Int -> Int -> Index
index3 i j k = ixN [i, j, k]
{-# INLINE index3 #-}
