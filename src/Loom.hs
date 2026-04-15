module Loom
  ( Arr
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
  , shape
  , withSchedule
  , forEach
  , for1
  , for2
  , for3
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

type Index = IxN

data Domain = Domain ![Int] !(Maybe Schedule)

shape :: [Int] -> Domain
shape dims = Domain dims Nothing
{-# INLINE shape #-}

infixl 5 `withSchedule`

withSchedule :: Domain -> Schedule -> Domain
withSchedule (Domain dims maybeSchedule) schedule =
  Domain dims (Just combined)
  where
    combined =
      case maybeSchedule of
        Nothing -> schedule
        Just current -> InternalSchedule.compose current schedule
{-# INLINE withSchedule #-}

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

for1 :: Int -> Schedule -> (Int -> Prog ()) -> Prog ()
for1 n schedule body =
  if InternalSchedule.isIdentitySchedule schedule
    then parFor n body
    else forScheduled [n] schedule $ \ix ->
      withIndex1 "for1" ix body
{-# INLINE for1 #-}

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

coords :: Index -> [Int]
coords = unIxN
{-# INLINE coords #-}

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
