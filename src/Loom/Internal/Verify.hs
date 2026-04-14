{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Loom.Internal.Verify
  ( Rank
  , Schedule
  , Capability
  , Shape
  , Array
  , Index
  , AccessCtx
  , Prog
  , Reducer
  , ReduceVar
  , shape1
  , shape2
  , extent1
  , extent2
  , newArray
  , fromList
  , toList
  , wrapArray
  , unwrapArray
  , parFor1D
  , parFor2D
  , foldFor1D
  , rectReadAccess1D
  , rectReadAccess2D
  , rectWriteAccess1D
  , rectWriteAccess2D
  , rectReadWriteAccess1D
  , rectReadWriteAccess2D
  , rectAccess1D
  , rectAccess2D
  , newReducer
  , reduce
  , getReducer
  , readAt
  , writeAt
  , rowOf
  , colOf
  , pairOf
  , unIndex1
  , unIndex2
  , runProg
  , parallel
  , barrier
  , intSum
  , doubleSum
  ) where

import Data.Primitive.Types (Prim)
import Loom.Internal.Kernel
  ( Arr
  , Ix1
  , Ix2
  , Prog
  , Reducer
  , Sh1
  , Sh2
  , barrier
  , doubleSum
  , foldFor
  , index1
  , index2
  , intSum
  , ix1
  , ix2
  , newArr
  , parForSh1
  , parForSh2
  , parallel
  , readArr
  , runProg
  , sh1
  , sh2
  , sizeOfArr
  , unIx1
  , unIx2
  , writeArr
  )
import qualified Loom.Internal.Kernel as K

data Rank
  = Rank1
  | Rank2

data Schedule
  = Rect

data Capability
  = ReadOnly
  | WriteOnly
  | ReadWrite

data Shape (rank :: Rank) where
  Shape1 :: !Int -> !Sh1 -> Shape 'Rank1
  Shape2 :: !Int -> !Int -> !Sh2 -> Shape 'Rank2

data Array (rank :: Rank) a where
  Array :: !(Shape rank) -> !(Arr a) -> Array rank a

data Index (sched :: Schedule) (rank :: Rank) where
  Index1 :: !Ix1 -> Index sched 'Rank1
  Index2 :: !Ix2 -> Index sched 'Rank2

data AccessCtx (cap :: Capability) (sched :: Schedule) (rank :: Rank) where
  Access1 :: !(Shape 'Rank1) -> AccessCtx cap sched 'Rank1
  Access2 :: !(Shape 'Rank2) -> AccessCtx cap sched 'Rank2

newtype ReduceVar a = ReduceVar (K.RedVar a)

class CanRead (cap :: Capability)

instance CanRead 'ReadOnly

instance CanRead 'ReadWrite

class CanWrite (cap :: Capability)

instance CanWrite 'WriteOnly

instance CanWrite 'ReadWrite

shape1 :: Int -> Shape 'Rank1
shape1 n = Shape1 n (sh1 n)

shape2 :: Int -> Int -> Shape 'Rank2
shape2 rows cols = Shape2 rows cols (sh2 rows cols)

extent1 :: Shape 'Rank1 -> Int
extent1 (Shape1 n _) = n

extent2 :: Shape 'Rank2 -> (Int, Int)
extent2 (Shape2 rows cols _) = (rows, cols)

newArray :: Prim a => Shape rank -> IO (Array rank a)
newArray shape = Array shape <$> newArr (shapeSize shape)

fromList :: Prim a => Shape rank -> [a] -> IO (Array rank a)
fromList shape xs
  | length xs == shapeSize shape = Array shape <$> K.fromList xs
  | otherwise =
      error
        ( "Loom.Verify.fromList: expected "
            ++ show (shapeSize shape)
            ++ " elements, got "
            ++ show (length xs)
        )

toList :: Prim a => Array rank a -> IO [a]
toList (Array _ arr) = K.toList arr

wrapArray :: Prim a => Shape rank -> Arr a -> Array rank a
wrapArray shape arr
  | shapeSize shape == sizeOfArr arr = Array shape arr
  | otherwise =
      error
        ( "Loom.Verify.wrapArray: expected array size "
            ++ show (shapeSize shape)
            ++ ", got "
            ++ show (sizeOfArr arr)
        )

unwrapArray :: Array rank a -> Arr a
unwrapArray (Array _ arr) = arr

parFor1D ::
  Shape 'Rank1 ->
  (AccessCtx 'ReadWrite 'Rect 'Rank1 -> Index 'Rect 'Rank1 -> Prog ()) ->
  Prog ()
parFor1D shape@(Shape1 _ rawShape) body =
  parForSh1 rawShape (\ix -> body (Access1 shape) (Index1 ix))

parFor2D ::
  Shape 'Rank2 ->
  (AccessCtx 'ReadWrite 'Rect 'Rank2 -> Index 'Rect 'Rank2 -> Prog ()) ->
  Prog ()
parFor2D shape@(Shape2 _ _ rawShape) body =
  parForSh2 rawShape (\ix -> body (Access2 shape) (Index2 ix))

foldFor1D :: Reducer a -> Shape 'Rank1 -> (Index 'Rect 'Rank1 -> Prog a) -> Prog a
foldFor1D reducer shape body =
  foldFor reducer (shapeSize shape) (\i -> body (Index1 (ix1 i)))

rectReadAccess1D :: Shape 'Rank1 -> AccessCtx 'ReadOnly 'Rect 'Rank1
rectReadAccess1D = Access1

rectReadAccess2D :: Shape 'Rank2 -> AccessCtx 'ReadOnly 'Rect 'Rank2
rectReadAccess2D = Access2

rectWriteAccess1D :: Shape 'Rank1 -> AccessCtx 'WriteOnly 'Rect 'Rank1
rectWriteAccess1D = Access1

rectWriteAccess2D :: Shape 'Rank2 -> AccessCtx 'WriteOnly 'Rect 'Rank2
rectWriteAccess2D = Access2

rectReadWriteAccess1D :: Shape 'Rank1 -> AccessCtx 'ReadWrite 'Rect 'Rank1
rectReadWriteAccess1D = Access1

rectReadWriteAccess2D :: Shape 'Rank2 -> AccessCtx 'ReadWrite 'Rect 'Rank2
rectReadWriteAccess2D = Access2

rectAccess1D :: Shape 'Rank1 -> AccessCtx 'ReadOnly 'Rect 'Rank1
rectAccess1D = rectReadAccess1D

rectAccess2D :: Shape 'Rank2 -> AccessCtx 'ReadOnly 'Rect 'Rank2
rectAccess2D = rectReadAccess2D

newReducer :: Reducer a -> (ReduceVar a -> Prog r) -> Prog r
newReducer reducer body =
  K.newReducer reducer (\redVar -> body (ReduceVar redVar))

reduce :: ReduceVar a -> a -> Prog ()
reduce (ReduceVar redVar) x = K.reduce redVar x

getReducer :: ReduceVar a -> Prog a
getReducer (ReduceVar redVar) = K.getReducer redVar

readAt ::
  (Prim a, CanRead cap) =>
  AccessCtx cap sched rank ->
  Array rank a ->
  Index sched rank ->
  Prog a
readAt ctx arr ix =
  case (ctx, arr, ix) of
    (Access1 ctxShape, Array arrShape rawArr, Index1 rawIx) ->
      checkMatchingShape "readAt" ctxShape arrShape `seq`
        readArr rawArr (index1 (rawShape1 arrShape) rawIx)
    (Access2 ctxShape, Array arrShape rawArr, Index2 rawIx) ->
      checkMatchingShape "readAt" ctxShape arrShape `seq`
        readArr rawArr (index2 (rawShape2 arrShape) rawIx)

writeAt ::
  (Prim a, CanWrite cap) =>
  AccessCtx cap sched rank ->
  Array rank a ->
  Index sched rank ->
  a ->
  Prog ()
writeAt ctx arr ix x =
  case (ctx, arr, ix) of
    (Access1 ctxShape, Array arrShape rawArr, Index1 rawIx) ->
      checkMatchingShape "writeAt" ctxShape arrShape `seq`
        writeArr rawArr (index1 (rawShape1 arrShape) rawIx) x
    (Access2 ctxShape, Array arrShape rawArr, Index2 rawIx) ->
      checkMatchingShape "writeAt" ctxShape arrShape `seq`
        writeArr rawArr (index2 (rawShape2 arrShape) rawIx) x

rowOf :: Index 'Rect 'Rank2 -> Index 'Rect 'Rank1
rowOf ix =
  case unIx2' ix of
    (row, _) -> Index1 (ix1 row)

colOf :: Index 'Rect 'Rank2 -> Index 'Rect 'Rank1
colOf ix =
  case unIx2' ix of
    (_, col) -> Index1 (ix1 col)

pairOf :: Index 'Rect 'Rank1 -> Index 'Rect 'Rank1 -> Index 'Rect 'Rank2
pairOf (Index1 row) (Index1 col) = Index2 (ix2 (unIx1 row) (unIx1 col))

unIndex1 :: Index sched 'Rank1 -> Int
unIndex1 (Index1 rawIx) = unIx1 rawIx

unIndex2 :: Index sched 'Rank2 -> (Int, Int)
unIndex2 = unIx2'

shapeSize :: Shape rank -> Int
shapeSize (Shape1 n _) = n
shapeSize (Shape2 rows cols _) = rows * cols

rawShape1 :: Shape 'Rank1 -> Sh1
rawShape1 (Shape1 _ rawShape) = rawShape

rawShape2 :: Shape 'Rank2 -> Sh2
rawShape2 (Shape2 _ _ rawShape) = rawShape

sameShape :: Shape rank -> Shape rank -> Bool
sameShape (Shape1 left _) (Shape1 right _) = left == right
sameShape (Shape2 leftRows leftCols _) (Shape2 rightRows rightCols _) =
  leftRows == rightRows && leftCols == rightCols

checkMatchingShape :: String -> Shape rank -> Shape rank -> ()
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
renderShape (Shape1 n _) = "sh1(" ++ show n ++ ")"
renderShape (Shape2 rows cols _) = "sh2(" ++ show rows ++ "," ++ show cols ++ ")"

unIx2' :: Index sched 'Rank2 -> (Int, Int)
unIx2' (Index2 rawIx) = unIx2 rawIx
