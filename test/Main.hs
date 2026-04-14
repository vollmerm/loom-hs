{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import GHC.Conc (getNumCapabilities)
import Loom
import qualified Loom.Polyhedral as Poly
import qualified Loom.Verify as Verify
import qualified Loom.Verify.Polyhedral as VerifyPoly
import Loom.Benchmark.Kernels
  ( runMatMulExample
  , runPolyhedralTiledMatMulExample
  , runPolyhedralWavefrontEditDistanceExample
  , runRedBlackStencilExample
  , runSeparableBlurExample
  , runThreePhaseNormalizeExample
  , runWavefrontEditDistanceExample
  )

main :: IO ()
main = do
  testArrayUpdate
  testIndexHelpers
  testAffine2Basics
  testShapeLoop1D
  testReducerSum
  testVerifiedFill
  testVerifiedMap
  testVerifiedSum
  testVerifiedDot
  testVerifiedMatMul
  testVerifiedShapeMismatch
  testVerifiedReducerScope
  testVerifiedThreePhaseNormalize
  testVerifiedTiledMatMul
  testVerifiedWavefrontEditDistance
  testAccumFor
  testVecPrimitives
  testStripMine
  testParallelReducerScope
  testDynamicChunkedReducerScope
  testAccumulatorPhases
  testBarrierRejectedInLoop
  testMatrixMultiply
  testRedBlackStencilBenchmark
  testThreePhaseNormalizeBenchmark
  testSeparableBlurBenchmark
  testAffineLoopIdentity
  testAffineLoopInterchange
  testAffineLoopSkew
  testAffineLoopComposition
  testAffineLoopRejectsSingularTransform
  testTiledRect2D
  testTransform2DIdentity
  testTransform2DTile
  testTransform2DAffineCompose
  testTransform2DGenericAffineTile
  testTransform2DRepeatedTile
  testTransform2DSkewTile
  testTransform2DSkewCompose
  testTransform2DSkewInterchange
  testTransform2DRejectsSingularAffine
  testTiledFor2D
  testTiledMatrixMultiply
  testPolyhedralAffineExprRendering
  testPolyhedralScheduleRendering
  testPolyhedralSummary
  testPolyhedralLegality
  testPolyhedralWavefrontLegality
  testPolyhedralLoweringPhases
  testPolyhedralInvalidLowering
  testPolyhedralTiledMatMul
  testPolyhedralWavefrontEditDistance
  testVerifiedPolyhedralBridge
  testWavefrontFillCoverage
  testWavefrontEdgeCases
  testWavefrontPascal
  testWavefrontEditDistanceBenchmark
  testMatMulBenchmarkVectorized
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

testVerifiedFill :: IO ()
testVerifiedFill = do
  let shape = Verify.shape1 6
  arr <- Verify.newArray shape
  Verify.runProg $
    Verify.parallel $
      Verify.parFor1D shape $ \ctx ix ->
        Verify.writeAt ctx arr ix (Verify.unIndex1 ix * 2)
  xs <- Verify.toList arr
  assertEqual "verified fill" [0, 2, 4, 6, 8, 10] xs

testVerifiedMap :: IO ()
testVerifiedMap = do
  let shape = Verify.shape1 5
  left <- Verify.fromList shape [1 .. 5 :: Int]
  right <- Verify.fromList shape [10, 20, 30, 40, 50 :: Int]
  out <- Verify.newArray shape
  Verify.runProg $
    Verify.parallel $
      Verify.parFor1D shape $ \ctx ix -> do
        x <- Verify.readAt ctx left ix
        y <- Verify.readAt ctx right ix
        Verify.writeAt ctx out ix (x + y)
  xs <- Verify.toList out
  assertEqual "verified map" [11, 22, 33, 44, 55] xs

testVerifiedSum :: IO ()
testVerifiedSum = do
  let shape = Verify.shape1 10
  arr <- Verify.fromList shape [1 .. 10 :: Int]
  total <- Verify.runProg $
    Verify.foldFor1D Verify.intSum shape $ \ix ->
      Verify.readAt (Verify.rectReadAccess1D shape) arr ix
  assertEqual "verified sum" 55 total

testVerifiedDot :: IO ()
testVerifiedDot = do
  let shape = Verify.shape1 4
  left <- Verify.fromList shape [1, 2, 3, 4 :: Int]
  right <- Verify.fromList shape [5, 6, 7, 8 :: Int]
  total <- Verify.runProg $
    Verify.foldFor1D Verify.intSum shape $ \ix -> do
      let ctx = Verify.rectReadAccess1D shape
      x <- Verify.readAt ctx left ix
      y <- Verify.readAt ctx right ix
      pure (x * y)
  assertEqual "verified dot" 70 total

testVerifiedMatMul :: IO ()
testVerifiedMatMul = do
  let shape = Verify.shape2 4 4
      kShape = Verify.shape1 4
  left <- Verify.fromList shape [1 .. 16 :: Int]
  right <-
    Verify.fromList
      shape
      [ 1, 0, 0, 0
      , 0, 1, 0, 0
      , 0, 0, 1, 0
      , 0, 0, 0, 1
      ]
  out <- Verify.newArray shape
  Verify.runProg $
    Verify.parallel $
      Verify.parFor2D shape $ \ctx outIx -> do
        let row = Verify.rowOf outIx
            col = Verify.colOf outIx
        total <-
          Verify.foldFor1D Verify.intSum kShape $ \k -> do
            lhs <- Verify.readAt ctx left (Verify.pairOf row k)
            rhs <- Verify.readAt ctx right (Verify.pairOf k col)
            pure (lhs * rhs)
        Verify.writeAt ctx out outIx total
  xs <- Verify.toList out
  assertEqual "verified matmul" [1 .. 16] xs

testVerifiedShapeMismatch :: IO ()
testVerifiedShapeMismatch = do
  let loopShape = Verify.shape1 3
      arrShape = Verify.shape1 4
  arr <- Verify.newArray arrShape
  result <-
    ( try $
        Verify.runProg $
          Verify.parallel $
            Verify.parFor1D loopShape $ \ctx ix ->
              Verify.writeAt ctx arr ix (0 :: Int)
    ) ::
      IO (Either SomeException ())
  case result of
    Left _ -> pure ()
    Right _ -> error "verified shape mismatch should fail"

testVerifiedReducerScope :: IO ()
testVerifiedReducerScope = do
  let shape = Verify.shape1 10
  total <- Verify.runProg $
    Verify.parallel $
      Verify.newReducer Verify.intSum $ \sumVar -> do
        Verify.parFor1D shape $ \_ ix ->
          Verify.reduce sumVar (Verify.unIndex1 ix + 1)
        Verify.barrier
        Verify.parFor1D shape $ \_ ix ->
          Verify.reduce sumVar (Verify.unIndex1 ix + 1)
        Verify.getReducer sumVar
  assertEqual "verified reducer scope" 110 total

testVerifiedThreePhaseNormalize :: IO ()
testVerifiedThreePhaseNormalize = do
  let shape = Verify.shape1 4
      input = [1.0, 2.0, 3.0, 4.0]
      count = fromIntegral (Verify.extent1 shape)
  src <- Verify.fromList shape input
  out <- Verify.newArray shape
  Verify.runProg $
    Verify.parallel $
      Verify.newReducer Verify.doubleSum $ \sumVar -> do
        Verify.parFor1D shape $ \ctx ix -> do
          x <- Verify.readAt ctx src ix
          Verify.reduce sumVar x
        total <- Verify.getReducer sumVar
        Verify.barrier
        let mean = total / count
        Verify.newReducer Verify.doubleSum $ \sqVar -> do
          Verify.parFor1D shape $ \ctx ix -> do
            x <- Verify.readAt ctx src ix
            let delta = x - mean
            Verify.reduce sqVar (delta * delta)
          sqTotal <- Verify.getReducer sqVar
          Verify.barrier
          let variance = sqTotal / count
              invStd
                | variance <= 0 = 0
                | otherwise = 1 / sqrt variance
          Verify.parFor1D shape $ \ctx ix -> do
            x <- Verify.readAt ctx src ix
            Verify.writeAt ctx out ix ((x - mean) * invStd)
  xs <- Verify.toList out
  assertApproxList "verified three-phase normalize" 1e-12 (expectedNormalized input) xs

testVerifiedTiledMatMul :: IO ()
testVerifiedTiledMatMul = do
  let shape = Verify.shape2 4 4
      kShape = Verify.shape1 4
  left <- Verify.fromList shape [1 .. 16 :: Int]
  right <-
    Verify.fromList
      shape
      [ 1, 0, 0, 0
      , 0, 1, 0, 0
      , 0, 0, 1, 0
      , 0, 0, 0, 1
      ]
  out <- Verify.newArray shape
  Verify.runProg $
    Verify.parallel $
      Verify.parForTiled2D 2 2 shape $ \ctx outIx -> do
        let row = Verify.rowOf outIx
            col = Verify.colOf outIx
            lhsCtx = Verify.rectReadAccess2D shape
            rhsCtx = Verify.rectReadAccess2D shape
        total <-
          Verify.foldFor1D Verify.intSum kShape $ \k -> do
            lhs <- Verify.readAt lhsCtx left (Verify.pairOf row k)
            rhs <- Verify.readAt rhsCtx right (Verify.pairOf k col)
            pure (lhs * rhs)
        Verify.writeAt ctx out outIx total
  xs <- Verify.toList out
  assertEqual "verified tiled matmul" [1 .. 16] xs

testVerifiedWavefrontEditDistance :: IO ()
testVerifiedWavefrontEditDistance = do
  let leftInput = [1, 2, 3, 2]
      rightInput = [1, 3, 2]
      leftShape = Verify.shape1 (length leftInput)
      rightShape = Verify.shape1 (length rightInput)
      waveShape = Verify.shape2 (length leftInput) (length rightInput)
      dpShape = Verify.shape2 (length leftInput + 1) (length rightInput + 1)
  left <- Verify.fromList leftShape leftInput
  right <- Verify.fromList rightShape rightInput
  dp <- Verify.newArray dpShape
  Verify.runProg $
    Verify.parallel $ do
      Verify.parFor1D (Verify.shape1 (length leftInput + 1)) $ \_ ix ->
        Verify.writeAt
          (Verify.rectReadWriteAccess2D dpShape)
          dp
          (Verify.pairOf ix (Verify.rectIx1 0))
          (Verify.unIndex1 ix)
      Verify.barrier
      Verify.parFor1D rightShape $ \_ ix -> do
        let j = Verify.unIndex1 ix + 1
        Verify.writeAt
          (Verify.rectReadWriteAccess2D dpShape)
          dp
          (Verify.rectIx2 0 j)
          j
      Verify.barrier
      Verify.parForWavefront2D waveShape $ \ctx ix -> do
        deletion <- Verify.readWaveAt ctx dp (1, 1) Verify.WavePrevRow ix
        insertion <- Verify.readWaveAt ctx dp (1, 1) Verify.WavePrevCol ix
        substitution <- Verify.readWaveAt ctx dp (1, 1) Verify.WavePrevDiag ix
        x <- Verify.readAt (Verify.rectReadAccess1D leftShape) left (Verify.rowOf ix)
        y <- Verify.readAt (Verify.rectReadAccess1D rightShape) right (Verify.colOf ix)
        let cost
              | x == y = 0
              | otherwise = 1
            cell = min (deletion + 1) (min (insertion + 1) (substitution + cost))
        Verify.writeWaveAt ctx dp (1, 1) ix cell
  xs <- Verify.toList dp
  baseline <- runWavefrontEditDistanceExample leftInput rightInput
  assertEqual "verified wavefront edit distance" baseline (last xs)

testAccumFor :: IO ()
testAccumFor = do
  arr <- fromList [1 .. 5 :: Int]
  total <- runProg $
    accumFor (sizeOfArr arr) 0 $ \acc i -> do
      x <- readArr arr i
      pure (acc + x * 2)
  assertEqual "accumFor" 30 total

testVecPrimitives :: IO ()
testVecPrimitives = do
  src <- fromList [1 .. 8 :: Int]
  out <- newArr 8
  runProg $ do
    left <- readVec src 0
    right <- readVec src 4
    writeVec out 0 (addVec (mulVec left (broadcastVec 2)) (broadcastVec 1))
    writeVec out 4 (addVec right (broadcastVec 10))
  xs <- toList out
  assertEqual "vec primitives" [3, 5, 7, 9, 15, 16, 17, 18] xs
  total <- runProg $ do
    vec <- readVec src 0
    pure (sumVec vec)
  assertEqual "vec reduction" 10 total

testStripMine :: IO ()
testStripMine = do
  src <- fromList [0 .. 9 :: Int]
  out <- newArr 10
  runProg $
    stripMine vecWidth 10
      (\base -> do
         vec <- readVec src base
         writeVec out base vec
      )
      (\i -> do
         x <- readArr src i
         writeArr out i x)
  xs <- toList out
  assertEqual "stripMine" [0 .. 9] xs

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

testDynamicChunkedReducerScope :: IO ()
testDynamicChunkedReducerScope = do
  caps <- getNumCapabilities
  let n = max 33 (caps * 8 + 3)
      expected = sum [i + (2 * i + 1) | i <- [0 .. n - 1]]
  total <- runProg $
    parallel $
      newReducer intSum $ \sumVar -> do
        parFor n $ \i ->
          reduce sumVar i
        barrier
        parFor n $ \i ->
          reduce sumVar (2 * i + 1)
        getReducer sumVar
  assertEqual "dynamic chunked reducer scope" expected total

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

testRedBlackStencilBenchmark :: IO ()
testRedBlackStencilBenchmark = do
  let n = 4
      input = [1 .. n * n]
  xs <- runRedBlackStencilExample n input
  assertEqual "red-black stencil benchmark" (expectedRedBlackStencil n input) xs

testThreePhaseNormalizeBenchmark :: IO ()
testThreePhaseNormalizeBenchmark = do
  let input = [1.0, 2.0, 3.0, 4.0]
  xs <- runThreePhaseNormalizeExample input
  assertApproxList "three-phase normalize benchmark" 1e-12 (expectedNormalized input) xs

testSeparableBlurBenchmark :: IO ()
testSeparableBlurBenchmark = do
  let n = 3
      input = [1 .. n * n]
  xs <- runSeparableBlurExample n input
  assertEqual "separable blur benchmark" (expectedSeparableBlur n input) xs

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

testTiledRect2D :: IO ()
testTiledRect2D = do
  let rows = 4
      cols = 5
      shape = sh2 rows cols
      rect = rect2 1 1 4 5
  arr <- fromList (replicate (rows * cols) (0 :: Int))
  runProg $
    parallel $
      tiledForRect2D 2 3 rect $ \ix ->
        withIx2 ix $ \i j ->
          writeArr arr (index2 shape ix) (i * 10 + j)
  xs <- toList arr
  assertEqual
    "tiled rect 2d"
    [ if i >= 1 && j >= 1
        then i * 10 + j
        else 0
    | i <- [0 .. rows - 1]
    , j <- [0 .. cols - 1]
    ]
    xs

testTransform2DIdentity :: IO ()
testTransform2DIdentity = do
  xs <- runTransformFill identityTransform2D
  assertEqual "transform 2d identity" expectedAffineFill xs

testTransform2DTile :: IO ()
testTransform2DTile = do
  xs <- runTransformFill (tileTransform2D 2 3)
  assertEqual "transform 2d tile" expectedAffineFill xs

testTransform2DAffineCompose :: IO ()
testTransform2DAffineCompose = do
  let transform =
        composeTransform2D
          (affineTransform2D interchange2D)
          (affineTransform2D (skew2D 1))
  xs <- runTransformFill transform
  assertEqual "transform 2d affine+affine" expectedAffineFill xs

testTransform2DGenericAffineTile :: IO ()
testTransform2DGenericAffineTile = do
  let transform =
        composeTransform2D
          (affineTransform2D (affine2 1 0 0 1 1 2))
          (tileTransform2D 2 3)
  xs <- runTransformFill transform
  assertEqual "transform 2d generic affine+tile" expectedAffineFill xs

testTransform2DRepeatedTile :: IO ()
testTransform2DRepeatedTile = do
  let transform =
        composeTransform2D
          (tileTransform2D 2 3)
          (tileTransform2D 1 2)
  xs <- runTransformFill transform
  assertEqual "transform 2d repeated tile" expectedAffineFill xs

testTransform2DSkewTile :: IO ()
testTransform2DSkewTile = do
  let transform =
        composeTransform2D
          (skewTransform2D 1)
          (tileTransform2D 2 3)
  xs <- runTransformFill transform
  assertEqual "transform 2d skew+tile" expectedAffineFill xs

testTransform2DSkewCompose :: IO ()
testTransform2DSkewCompose = do
  let transform =
        composeTransform2D
          (skewTransform2D 1)
          (skewTransform2D 2)
  xs <- runTransformFill transform
  assertEqual "transform 2d skew+skew" expectedAffineFill xs

testTransform2DSkewInterchange :: IO ()
testTransform2DSkewInterchange = do
  let transform =
        composeTransform2D
          (skewTransform2D 1)
          interchangeTransform2D
  xs <- runTransformFill transform
  assertEqual "transform 2d skew+interchange" expectedAffineFill xs

testTransform2DRejectsSingularAffine :: IO ()
testTransform2DRejectsSingularAffine = do
  result <-
    ( try $
        runProg $
          parallel $
            parForTransform2D
              (affineTransform2D (affine2 1 0 0 0 0 0))
              (sh2 2 2)
              (\_ -> pure ())
    ) ::
      IO (Either SomeException ())
  case result of
    Left _ -> pure ()
    Right _ -> error "singular transform stage should fail"

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

testPolyhedralAffineExprRendering :: IO ()
testPolyhedralAffineExprRendering = do
  let expr =
        Poly.plus
          (Poly.scaled 3 Poly.rowVar)
          (Poly.plus
             (Poly.scaled (-2) Poly.colVar)
             (Poly.plus (Poly.auxVar "k") (Poly.constant 5)))
      shifted =
        Poly.plus Poly.rowVar (Poly.constant (-3))
  assertEqual "polyhedral affine expr render" "3*row - 2*col + k + 5" (Poly.renderAffineExpr expr)
  assertEqual "polyhedral affine expr negative offset" "row - 3" (Poly.renderAffineExpr shifted)

testPolyhedralScheduleRendering :: IO ()
testPolyhedralScheduleRendering = do
  let schedule =
        Poly.composeSchedule2D
          (Poly.affineSchedule2D interchange2D)
          (Poly.composeSchedule2D Poly.identitySchedule2D (Poly.tileSchedule2D 2 3))
  assertEqual "polyhedral identity schedule render" "identity" (Poly.renderSchedule2D Poly.identitySchedule2D)
  assertEqual "polyhedral composed schedule render" "affine -> tile(2,3)" (Poly.renderSchedule2D schedule)

testPolyhedralSummary :: IO ()
testPolyhedralSummary = do
  let access =
        Poly.readAccess2D
          "src"
          (Poly.plus (Poly.scaled 4 Poly.rowVar) Poly.colVar)
      kernel =
        Poly.kernel2D
          (sh2 4 4)
          [ Poly.phase2D
              "summary"
              (Poly.composeSchedule2D Poly.identitySchedule2D (Poly.tileSchedule2D 2 3))
              Poly.IndependentDependence2D
              [access]
              [Poly.writeAccess2D "dst" (Poly.plus (Poly.scaled 4 Poly.rowVar) Poly.colVar)]
              (\_ _ -> pure ())
          ]
  case Poly.validateKernel2D kernel of
    Left err -> error ("unexpected polyhedral validation failure: " ++ show err)
    Right summary ->
      case Poly.kernelSummaryPhases summary of
        [phaseSummary] -> do
          assertEqual "polyhedral schedule summary" "tile(2,3)" (Poly.renderSchedule2D (Poly.phaseSummarySchedule phaseSummary))
          assertEqual "polyhedral access summary" "read src[4*row + col]" (Poly.renderAccess2D access)
        _ -> error "expected one polyhedral phase summary"

testPolyhedralLegality :: IO ()
testPolyhedralLegality = do
  let singularKernel =
        Poly.kernel2D
          (sh2 2 2)
          [ Poly.phase2D
              "singular"
              (Poly.affineSchedule2D (affine2 1 0 0 0 0 0))
              Poly.IndependentDependence2D
              []
              []
              (\_ _ -> pure ())
          ]
      opaqueKernel =
        Poly.kernel2D
          (sh2 2 2)
          [ Poly.phase2D
              "opaque"
              (Poly.tileSchedule2D 2 2)
              (Poly.OpaqueDependence2D "stencil-carried dependence")
              []
              []
              (\_ _ -> pure ())
          ]
  case Poly.validateKernel2D singularKernel of
    Left err ->
      assertEqual
        "polyhedral singular affine rejected"
        (Poly.InvalidSchedule2D "affine schedule stages must be invertible integer transforms")
        err
    Right _ ->
      error "singular polyhedral schedule should fail"
  case Poly.validateKernel2D opaqueKernel of
    Left err ->
      assertEqual
        "polyhedral opaque dependence rejected"
        (Poly.IllegalDependence2D "opaque dependences are only accepted with the identity schedule: stencil-carried dependence")
        err
    Right _ ->
      error "opaque polyhedral dependence should fail"

testPolyhedralWavefrontLegality :: IO ()
testPolyhedralWavefrontLegality = do
  let invalidKernel =
        Poly.kernel2D
          (sh2 3 3)
          [ Poly.phase2D
              "wavefront+tiled"
              (Poly.composeSchedule2D Poly.wavefrontSchedule2D (Poly.tileSchedule2D 2 2))
              Poly.WavefrontDependence2D
              []
              []
              (\_ _ -> pure ())
          ]
      identityOpaqueKernel =
        Poly.kernel2D
          (sh2 2 2)
          [ Poly.phase2D
              "opaque-identity"
              Poly.identitySchedule2D
              (Poly.OpaqueDependence2D "allowed on identity")
              []
              []
              (\_ _ -> pure ())
          ]
  case Poly.validateKernel2D invalidKernel of
    Left err ->
      assertEqual
        "polyhedral wavefront schedule rejected"
        (Poly.IllegalDependence2D "wavefront dependences require the wavefront schedule in the MVP polyhedral subset")
        err
    Right _ ->
      error "invalid wavefront polyhedral schedule should fail"
  case Poly.validateKernel2D identityOpaqueKernel of
    Left err ->
      error ("identity opaque polyhedral schedule should validate: " ++ show err)
    Right _ ->
      pure ()

testPolyhedralLoweringPhases :: IO ()
testPolyhedralLoweringPhases = do
  arr <- newArr 6
  let kernel =
        Poly.kernel2D
          (sh2 2 3)
          [ Poly.phase2D
              "fill"
              Poly.identitySchedule2D
              Poly.IndependentDependence2D
              []
              [Poly.writeAccess2D "arr" (Poly.plus (Poly.scaled 3 Poly.rowVar) Poly.colVar)]
              (\i j -> writeArr arr (i * 3 + j) (i * 10 + j))
          , Poly.phase2D
              "bump"
              (Poly.tileSchedule2D 1 2)
              Poly.IndependentDependence2D
              [Poly.readAccess2D "arr" (Poly.plus (Poly.scaled 3 Poly.rowVar) Poly.colVar)]
              [Poly.writeAccess2D "arr" (Poly.plus (Poly.scaled 3 Poly.rowVar) Poly.colVar)]
              (\i j -> do
                 x <- readArr arr (i * 3 + j)
                 writeArr arr (i * 3 + j) (x + 1))
          ]
  case Poly.lowerKernel2D kernel of
    Left err ->
      error ("unexpected polyhedral lowering failure: " ++ show err)
    Right prog ->
      runProg (parallel prog)
  xs <- toList arr
  assertEqual "polyhedral lowering phases" [1, 2, 3, 11, 12, 13] xs

testPolyhedralInvalidLowering :: IO ()
testPolyhedralInvalidLowering = do
  let kernel =
        Poly.kernel2D
          (sh2 2 2)
          [ Poly.phase2D
              "bad-tile"
              (Poly.tileSchedule2D 0 2)
              Poly.IndependentDependence2D
              []
              []
              (\_ _ -> pure ())
          ]
  case Poly.lowerKernel2D kernel of
    Left err ->
      assertEqual
        "polyhedral invalid lowering rejected"
        (Poly.InvalidSchedule2D "tile schedule stages require a positive row tile size")
        err
    Right _ ->
      error "invalid polyhedral lowering should fail"

testPolyhedralTiledMatMul :: IO ()
testPolyhedralTiledMatMul = do
  let n = 4
      left = [1 .. n * n]
      right =
        [ 1, 0, 2, 1
        , 0, 1, 1, 2
        , 2, 1, 0, 1
        , 1, 2, 1, 0
        ]
  xs <- runPolyhedralTiledMatMulExample n left right
  ys <- runMatMulExample n left right
  assertEqual "polyhedral tiled matmul" ys xs

testPolyhedralWavefrontEditDistance :: IO ()
testPolyhedralWavefrontEditDistance = do
  let left = [1, 2, 3, 2]
      right = [1, 3, 2]
  distance <- runPolyhedralWavefrontEditDistanceExample left right
  baseline <- runWavefrontEditDistanceExample left right
  assertEqual "polyhedral wavefront edit distance" baseline distance

testVerifiedPolyhedralBridge :: IO ()
testVerifiedPolyhedralBridge = do
  assertEqual
    "verified polyhedral tile schedule"
    "tile(2,3)"
    (Poly.renderSchedule2D (VerifyPoly.verifiedSchedule2D (VerifyPoly.TileSchedule2D 2 3)))
  assertEqual
    "verified polyhedral wave write"
    "write dp[5*row + col + 6]"
    (Poly.renderAccess2D (VerifyPoly.waveRowMajorWrite2D "dp" 5 (1, 1)))
  assertEqual
    "verified polyhedral wave prevdiag"
    "read dp[5*row + col]"
    (Poly.renderAccess2D (VerifyPoly.waveRowMajorRead2D "dp" 5 (1, 1) Verify.WavePrevDiag))

testWavefrontFillCoverage :: IO ()
testWavefrontFillCoverage = do
  let rows = 4
      cols = 5
      shape = sh2 rows cols
  arr <- fromList (replicate (rows * cols) (-1 :: Int))
  runProg $
    parallel $
      parForWavefront2D shape $ \ix ->
        withIx2 ix $ \i j ->
          writeArr arr (index2 shape ix) (i * 10 + j)
  xs <- toList arr
  assertEqual
    "wavefront fill coverage"
    [i * 10 + j | i <- [0 .. rows - 1], j <- [0 .. cols - 1]]
    xs

testWavefrontEdgeCases :: IO ()
testWavefrontEdgeCases = do
  emptyRows <-
    runProg $
      parallel $
        newReducer intSum $ \sumVar -> do
          parForWavefront2D (sh2 0 4) $ \_ ->
            reduce sumVar 1
          getReducer sumVar
  emptyCols <-
    runProg $
      parallel $
        newReducer intSum $ \sumVar -> do
          parForWavefront2D (sh2 4 0) $ \_ ->
            reduce sumVar 1
          getReducer sumVar
  singleton <-
    runProg $
      parallel $
        newReducer intSum $ \sumVar -> do
          parForWavefront2D (sh2 1 1) $ \_ ->
            reduce sumVar 1
          getReducer sumVar
  assertEqual "wavefront empty rows" 0 emptyRows
  assertEqual "wavefront empty cols" 0 emptyCols
  assertEqual "wavefront singleton" 1 singleton

testWavefrontPascal :: IO ()
testWavefrontPascal = do
  let rows = 5
      cols = 6
  arr <- newArr (rows * cols)
  runProg $
    parallel $ do
      parFor rows $ \i ->
        writeArr arr (i * cols) 1
      barrier
      parFor (cols - 1) $ \j0 ->
        let j = j0 + 1
         in writeArr arr j 1
      barrier
      parForWavefront2D (sh2 (rows - 1) (cols - 1)) $ \ix ->
        withIx2 ix $ \i0 j0 -> do
          let i = i0 + 1
              j = j0 + 1
          up <- readArr arr ((i - 1) * cols + j)
          left <- readArr arr (i * cols + (j - 1))
          writeArr arr (i * cols + j) (up + left)
  xs <- toList arr
  assertEqual "wavefront pascal" (expectedWavefrontPascal rows cols) xs

testWavefrontEditDistanceBenchmark :: IO ()
testWavefrontEditDistanceBenchmark = do
  distance <- runWavefrontEditDistanceExample [1, 2, 3, 4] [1, 3, 4]
  assertEqual "wavefront edit distance benchmark" 1 distance

testMatMulBenchmarkVectorized :: IO ()
testMatMulBenchmarkVectorized = do
  xs <-
    runMatMulExample
      4
      [1 .. 16]
      [ 1, 0, 0, 0
      , 0, 1, 0, 0
      , 0, 0, 1, 0
      , 0, 0, 0, 1
      ]
  assertEqual "matmul benchmark vectorized" [1 .. 16] xs

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

runTransformFill :: Transform2D -> IO [Int]
runTransformFill transform = do
  let rows = 3
      cols = 5
      shape = sh2 rows cols
  arr <- newArr (rows * cols)
  runProg $
    parallel $
      parForTransform2D transform shape $ \ix ->
        withIx2 ix $ \i j ->
          writeArr arr (index2 shape ix) (i * 10 + j)
  toList arr

expectedAffineFill :: [Int]
expectedAffineFill = [i * 10 + j | i <- [0 .. 2], j <- [0 .. 4]]

expectedRedBlackStencil :: Int -> [Int] -> [Int]
expectedRedBlackStencil n input = blackPhase
  where
    redPhase =
      [ if isInteriorCell n i j && even (i + j)
          then stencilValue input i j
          else input !! idx
      | i <- [0 .. n - 1]
      , j <- [0 .. n - 1]
      , let idx = i * n + j
      ]
    blackPhase =
      [ if isInteriorCell n i j && odd (i + j)
          then stencilValue redPhase i j
          else redPhase !! idx
      | i <- [0 .. n - 1]
      , j <- [0 .. n - 1]
      , let idx = i * n + j
      ]
    stencilValue xs i j =
      ( xs !! (i * n + j)
          + xs !! ((i - 1) * n + j)
          + xs !! ((i + 1) * n + j)
          + xs !! (i * n + (j - 1))
          + xs !! (i * n + (j + 1))
      )
        `quot` 5

expectedNormalized :: [Double] -> [Double]
expectedNormalized xs = map (\x -> (x - mean) * invStd) xs
  where
    n = fromIntegral (length xs)
    mean = sum xs / n
    variance = sum [delta * delta | x <- xs, let delta = x - mean] / n
    invStd
      | variance <= 0 = 0
      | otherwise = 1 / sqrt variance

expectedSeparableBlur :: Int -> [Int] -> [Int]
expectedSeparableBlur n input =
  [ ( horizontal !! (clampCell n (i - 1) * n + j)
        + horizontal !! (i * n + j)
        + horizontal !! (clampCell n (i + 1) * n + j)
    )
      `quot` 3
  | i <- [0 .. n - 1]
  , j <- [0 .. n - 1]
  ]
  where
    horizontal =
      [ ( input !! (i * n + clampCell n (j - 1))
            + input !! (i * n + j)
            + input !! (i * n + clampCell n (j + 1))
        )
          `quot` 3
      | i <- [0 .. n - 1]
      , j <- [0 .. n - 1]
      ]

expectedWavefrontPascal :: Int -> Int -> [Int]
expectedWavefrontPascal rows cols =
  goRows 0 []
  where
    goRows i builtRowsRev
      | i >= rows = concat (reverse builtRowsRev)
      | otherwise =
          let prevRow =
                case builtRowsRev of
                  priorRow : _ -> priorRow
                  [] -> []
              row = buildRow i prevRow 0 []
           in goRows (i + 1) (row : builtRowsRev)

    buildRow i prevRow j rowRev
      | j >= cols = reverse rowRev
      | i == 0 || j == 0 = buildRow i prevRow (j + 1) (1 : rowRev)
      | otherwise =
          let up = prevRow !! j
              left =
                case rowRev of
                  leftValue : _ -> leftValue
                  [] -> error "wavefront pascal row prefix unexpectedly empty"
              !cell = up + left
           in buildRow i prevRow (j + 1) (cell : rowRev)

isInteriorCell :: Int -> Int -> Int -> Bool
isInteriorCell n i j = i > 0 && i + 1 < n && j > 0 && j + 1 < n

clampCell :: Int -> Int -> Int
clampCell n i
  | i < 0 = 0
  | i >= n = n - 1
  | otherwise = i

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

assertApproxList :: String -> Double -> [Double] -> [Double] -> IO ()
assertApproxList label tolerance expected actual = do
  assertEqual (label <> " length") (length expected) (length actual)
  unless (and (zipWith (\x y -> abs (x - y) <= tolerance) expected actual)) $
    error
      ( label
          <> " failed: expected "
          <> show expected
          <> ", got "
          <> show actual
      )
