{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Loom

main :: IO ()
main = do
  testArrayUpdate
  testIndexHelpers
  testAffine2Basics
  testShapeLoop1D
  testReducerSum
  testAccumFor
  testParallelReducerScope
  testAccumulatorPhases
  testBarrierRejectedInLoop
  testMatrixMultiply
  testAffineLoopIdentity
  testAffineLoopInterchange
  testAffineLoopSkew
  testAffineLoopComposition
  testAffineLoopRejectsSingularTransform
  testTiledFor2D
  testTiledMatrixMultiply
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

testAffine2Basics :: IO ()
testAffine2Basics = do
  let skew = skew2D 1
      shift = affine2 1 0 0 1 2 (-1)
      composed = composeAffine2 shift skew
      box = boundingBoxAffine2D skew (rect2 0 0 3 4)
  assertEqual "apply affine2" (2, 5) (unIx2 (applyAffine2 skew (ix2 2 3)))
  assertEqual "compose affine2" (4, 4) (unIx2 (applyAffine2 composed (ix2 2 3)))
  assertEqual "invert affine2" (Just (2, 3)) (fmap (unIx2 . (`applyAffine2` ix2 4 4)) (invertAffine2 composed))
  assertEqual "bounding box affine2d" (0, 0, 3, 6) (unRect2 box)

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

testAffineLoopIdentity :: IO ()
testAffineLoopIdentity = do
  xs <- runAffineFill identityAffine2
  assertEqual "affine loop identity" expectedAffineFill xs

testAffineLoopInterchange :: IO ()
testAffineLoopInterchange = do
  xs <- runAffineFill interchange2D
  assertEqual "affine loop interchange" expectedAffineFill xs

testAffineLoopSkew :: IO ()
testAffineLoopSkew = do
  xs <- runAffineFill (skew2D 1)
  assertEqual "affine loop skew" expectedAffineFill xs

testAffineLoopComposition :: IO ()
testAffineLoopComposition = do
  let transform = composeAffine2 interchange2D (skew2D 1)
  xs <- runAffineFill transform
  assertEqual "affine loop composition" expectedAffineFill xs

testAffineLoopRejectsSingularTransform :: IO ()
testAffineLoopRejectsSingularTransform = do
  result <-
    ( try $
        runProg $
          parallel $
            parForAffine2D (affine2 1 0 0 0 0 0) (sh2 2 2) $ \_ ->
              pure ()
    ) ::
      IO (Either SomeException ())
  case result of
    Left _ -> pure ()
    Right _ -> error "singular affine transform should fail"

testTiledFor2D :: IO ()
testTiledFor2D = do
  let rows = 3
      cols = 5
      shape = sh2 rows cols
  arr <- newArr (rows * cols)
  runProg $
    parallel $
      tiledFor2D 2 3 shape $ \i j ->
        writeArr arr (i * cols + j) (i * 10 + j)
  xs <- toList arr
  assertEqual "tiled for 2d" [i * 10 + j | i <- [0 .. rows - 1], j <- [0 .. cols - 1]] xs

testTiledMatrixMultiply :: IO ()
testTiledMatrixMultiply = do
  let rowsA = 2
      colsA = 3
      colsB = 2
      outShape = sh2 rowsA colsB
  a <- fromList [1, 2, 3, 4, 5, 6 :: Int]
  b <- fromList [7, 8, 9, 10, 11, 12 :: Int]
  c <- newArr (rowsA * colsB)
  runProg $
    parallel $
      tile2D 2 1 outShape $ \row0 col0 ->
        parForTile2D 2 1 row0 col0 outShape $ \i j -> do
          total <- accumFor colsA 0 $ \sumVar k -> do
            lhs <- readArr a (i * colsA + k)
            rhs <- readArr b (k * colsB + j)
            pure (sumVar + lhs * rhs)
          writeArr c (i * colsB + j) total
  xs <- toList c
  assertEqual "tiled matrix multiply" [58, 64, 139, 154] xs

runAffineFill :: Affine2 -> IO [Int]
runAffineFill transform = do
  let rows = 3
      cols = 5
      shape = sh2 rows cols
  arr <- newArr (rows * cols)
  runProg $
    parallel $
      parForAffine2D transform shape $ \ix ->
        withIx2 ix $ \i j ->
          writeArr arr (index2 shape ix) (i * 10 + j)
  toList arr

expectedAffineFill :: [Int]
expectedAffineFill = [i * 10 + j | i <- [0 .. 2], j <- [0 .. 4]]

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
