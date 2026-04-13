{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Loom

main :: IO ()
main = do
  testArrayUpdate
  testIndexHelpers
  testShapeLoop1D
  testReducerSum
  testAccumFor
  testParallelReducerScope
  testAccumulatorPhases
  testBarrierRejectedInLoop
  testMatrixMultiply
  putStrLn "All loom-hs tests passed."

testArrayUpdate :: IO ()
testArrayUpdate = do
  arr <- fromList [1, 2, 3, 4 :: Int]
  runProg $
    parFor (sizeOfArr arr) $ \i -> do
      x <- readArr arr i
      writeArr arr i (x + 1)
  xs <- toList arr
  assertEqual "array update" [2, 3, 4, 5] xs

testIndexHelpers :: IO ()
testIndexHelpers = do
  assertEqual "unIx1" 3 (unIx1 (ix1 3))
  assertEqual "unIx2" (2, 1) (unIx2 (ix2 2 1))
  assertEqual "index1" 4 (index1 (sh1 8) (ix1 4))
  assertEqual "index2" 9 (index2 (sh2 3 4) (ix2 2 1))

testShapeLoop1D :: IO ()
testShapeLoop1D = do
  let shape = sh1 6
  arr <- newArr 6
  runProg $
    parForSh1 shape $ \ix ->
      writeArr arr (index1 shape ix) (unIx1 ix * 2)
  xs <- toList arr
  assertEqual "shape loop 1d" [0, 2, 4, 6, 8, 10] xs

testReducerSum :: IO ()
testReducerSum = do
  arr <- fromList [1 .. 10 :: Int]
  total <- runProg $
    foldFor intSum (sizeOfArr arr) $ \i ->
      readArr arr i
  assertEqual "reducer sum" 55 total

testAccumFor :: IO ()
testAccumFor = do
  arr <- fromList [1 .. 5 :: Int]
  total <- runProg $
    accumFor (sizeOfArr arr) 0 $ \acc i -> do
      x <- readArr arr i
      pure (acc + x * 2)
  assertEqual "accumFor" 30 total

testParallelReducerScope :: IO ()
testParallelReducerScope = do
  total <- runProg $
    parallel $
      newReducer intSum $ \sumVar -> do
        parFor 10 $ \i ->
          reduce sumVar (i + 1)
        barrier
        parFor 10 $ \i ->
          reduce sumVar (i + 1)
        getReducer sumVar
  assertEqual "parallel reducer scope" 110 total

testAccumulatorPhases :: IO ()
testAccumulatorPhases = do
  arr <- newArr 6
  runProg $
    parallel $ do
      parFor 6 $ \i ->
        writeArr arr i i
      barrier
      parFor 6 $ \i ->
        newAcc (0 :: Int) $ \tmp -> do
          x <- readArr arr i
          writeAcc tmp (x * 3)
          y <- readAcc tmp
          writeArr arr i (y + 1)
  xs <- toList arr
  assertEqual "accumulator phases" [1, 4, 7, 10, 13, 16] xs

testBarrierRejectedInLoop :: IO ()
testBarrierRejectedInLoop = do
  result <-
    ( try $
        runProg $
          parallel $
            parFor 4 $ \_ ->
              barrier
    ) ::
      IO (Either SomeException ())
  case result of
    Left _ -> pure ()
    Right _ -> error "barrier in loop should fail"

testMatrixMultiply :: IO ()
testMatrixMultiply = do
  let rowsA = 2
      colsA = 3
      colsB = 2
      outShape = sh2 rowsA colsB
  a <- fromList [1, 2, 3, 4, 5, 6 :: Int]
  b <- fromList [7, 8, 9, 10, 11, 12 :: Int]
  c <- newArr (rowsA * colsB)
  runProg $
    parallel $
      parForSh2 outShape $ \outIx ->
        withIx2 outIx $ \i j -> do
          total <- accumFor colsA 0 $ \sumVar k -> do
            lhs <- readArr a (i * colsA + k)
            rhs <- readArr b (k * colsB + j)
            pure (sumVar + lhs * rhs)
          writeArr c (index2 outShape outIx) total
  xs <- toList c
  assertEqual "matrix multiply" [58, 64, 139, 154] xs

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  unless (expected == actual) $
    error
      ( label
          <> " failed: expected "
          <> show expected
          <> ", got "
          <> show actual
      )
