{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Monad (unless)
import Loom

main :: IO ()
main = do
  testArrayUpdate
  testReducerSum
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

testReducerSum :: IO ()
testReducerSum = do
  arr <- fromList [1 .. 10 :: Int]
  total <- runProg $
    foldFor intSum (sizeOfArr arr) $ \i ->
      readArr arr i
  assertEqual "reducer sum" 55 total

testMatrixMultiply :: IO ()
testMatrixMultiply = do
  let rowsA = 2
      colsA = 3
      colsB = 2
  a <- fromList [1, 2, 3, 4, 5, 6 :: Int]
  b <- fromList [7, 8, 9, 10, 11, 12 :: Int]
  c <- newArr (rowsA * colsB)
  runProg $
    parFor (rowsA * colsB) $ \idx -> do
      let (!i, !j) = idx `divMod` colsB
      total <- foldFor intSum colsA $ \k -> do
          lhs <- readArr a (i * colsA + k)
          rhs <- readArr b (k * colsB + j)
          pure (lhs * rhs)
      writeArr c idx total
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
