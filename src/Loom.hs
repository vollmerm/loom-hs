{-# LANGUAGE GADTs #-}

module Loom
  ( Arr
  , Index
  , Loop
  , Schedule
  , Vec
  , IVec
  , I32Vec
  , DVec
  , Prog
  , Reducer
  , RedVar
  , AccVar
  , range
  , grid
  , grid2
  , grid3
  , scheduled
  , for
  , coords
  , linearIndex
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
  , fromList
  , getReducer
  , int32Sum
  , intSum
  , mkReducer
  , mkReducerWith
  , mulDVec
  , mulI32Vec
  , mulIVec
  , mulVec
  , newAcc
  , newArr
  , newReducer
  , parFor
  , parFor2
  , parFor3
  , parForScheduleN
  , parForShN
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
  , sizeOfArr
  , stripMine
  , sumDVec
  , sumI32Vec
  , sumIVec
  , sumVec
  , toList
  , unIxN
  , vecWidth
  , writeAcc
  , writeArr
  , writeArrIO
  , writeDVec
  , writeI32Vec
  , writeIVec
  , writeVec
  , indexN
  )
import qualified Loom.Schedule as Schedule
import Loom.Schedule (Schedule)

type Index = IxN

data Loop ix where
  Range :: !Int -> Loop Int
  Grid2 :: !Int -> !Int -> Loop (Int, Int)
  Grid3 :: !Int -> !Int -> !Int -> Loop (Int, Int, Int)
  GridN :: [Int] -> Loop Index
  Scheduled :: !Schedule -> Loop ix -> Loop ix

range :: Int -> Loop Int
range = Range
{-# INLINE range #-}

grid :: [Int] -> Loop Index
grid = GridN
{-# INLINE grid #-}

grid2 :: Int -> Int -> Loop (Int, Int)
grid2 = Grid2
{-# INLINE grid2 #-}

grid3 :: Int -> Int -> Int -> Loop (Int, Int, Int)
grid3 = Grid3
{-# INLINE grid3 #-}

scheduled :: Schedule -> Loop ix -> Loop ix
scheduled = Scheduled
{-# INLINE scheduled #-}

for :: Loop ix -> (ix -> Prog ()) -> Prog ()
for loop body =
  case loop of
    Range n -> parFor n body
    Grid2 rows cols -> parFor2 rows cols (\i j -> body (i, j))
    Grid3 depth rows cols -> parFor3 depth rows cols (\i j k -> body (i, j, k))
    GridN dims -> parForShN (shN dims) body
    Scheduled sched inner -> forScheduled sched inner body
{-# INLINE for #-}

coords :: Index -> [Int]
coords = unIxN
{-# INLINE coords #-}

linearIndex :: [Int] -> Index -> Int
linearIndex dims = indexN (shN dims)
{-# INLINE linearIndex #-}

forScheduled :: Schedule -> Loop ix -> (ix -> Prog ()) -> Prog ()
forScheduled sched loop body =
  case loop of
    Range n ->
      parForScheduleN sched (shN [n]) $ \ix ->
        case unIxN ix of
          [i] -> body i
          coords' -> error ("Loom.for: expected a 1D scheduled index, got " <> show coords')
    Grid2 rows cols ->
      parForScheduleN sched (shN [rows, cols]) $ \ix ->
        case unIxN ix of
          [i, j] -> body (i, j)
          coords' -> error ("Loom.for: expected a 2D scheduled index, got " <> show coords')
    Grid3 depth rows cols ->
      parForScheduleN sched (shN [depth, rows, cols]) $ \ix ->
        case unIxN ix of
          [i, j, k] -> body (i, j, k)
          coords' -> error ("Loom.for: expected a 3D scheduled index, got " <> show coords')
    GridN dims ->
      parForScheduleN sched (shN dims) body
    Scheduled innerSched inner ->
      forScheduled (Schedule.compose innerSched sched) inner body
{-# INLINE forScheduled #-}
