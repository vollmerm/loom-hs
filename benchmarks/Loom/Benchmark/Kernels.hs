{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Loom.Benchmark.Kernels
  ( Benchmark (..)
  , benchmarks
  , lookupBenchmark
  , runFill3DExample
  , runJacobi1DExample
  , runVerifiedJacobi1DExample
  , runNBodyExample
  , runInt32TiledMatMulScalarExample
  , runInt32TiledMatMulVectorizedExample
  , runDoubleMatMulExample
  , runDoubleTiledMatMulScalarExample
  , runDoubleTiledMatMulVectorizedExample
  , runDoubleTiledMatMulVectorizedNewApiExample
  , runMatMulExample
  , runPolyhedralTiledMatMulExample
  , runPolyhedralWavefrontEditDistanceExample
  , runWavefrontEditDistanceExample
  , runRedBlackStencilExample
  , runThreePhaseNormalizeExample
  , runSeparableBlurExample
  , runWavefrontLCSExample
  , runJacobi2DExample
  , runTiled3DMapExample
  ) where

import Loom
import Loom.Expert
import qualified Loom.Polyhedral as Poly
import qualified Loom.Schedule as Schedule
import qualified Loom.Verify as Verify
import qualified Loom.Verify.Polyhedral as VerifyPoly
import Data.Int (Int32)

data Benchmark = forall env. Benchmark
  { benchmarkName :: String
  , benchmarkDescription :: String
  , benchmarkDefaultSize :: Int
  , benchmarkSetup :: Int -> IO env
  , benchmarkPrepare :: env -> IO ()
  , benchmarkRun :: env -> IO BenchmarkRunResult
  , benchmarkValidate :: env -> BenchmarkRunResult -> IO Int
  }

data BenchmarkRunResult
  = BenchmarkNoChecksum
  | BenchmarkChecksum !Int

data FillEnv = FillEnv
  { fillLength :: !Int
  , fillOutput :: !(Arr Int)
  }

data Fill3DEnv = Fill3DEnv
  { fill3DLength :: !Int
  , fill3DOutput :: !(Arr Int)
  }

data UnaryEnv = UnaryEnv
  { unaryLength :: !Int
  , unaryInput :: !(Arr Int)
  }

data BinaryEnv = BinaryEnv
  { binaryLength :: !Int
  , binaryLeft :: !(Arr Int)
  , binaryRight :: !(Arr Int)
  , binaryOutput :: !(Arr Int)
  }

data MatrixEnv = MatrixEnv
  { matrixDim :: !Int
  , matrixLeft :: !(Arr Int)
  , matrixRight :: !(Arr Int)
  , matrixOutput :: !(Arr Int)
  }

data ImageEnv = ImageEnv
  { imageDim :: !Int
  , imageInput :: !(Arr Int)
  , imageOutput :: !(Arr Int)
  , imageScratch :: !(Arr Int)
  }

data DoubleUnaryEnv = DoubleUnaryEnv
  { doubleLength :: !Int
  , doubleInput :: !(Arr Double)
  , doubleOutput :: !(Arr Double)
  }

data JacobiEnv = JacobiEnv
  { jacobiLength :: !Int
  , jacobiSteps :: !Int
  , jacobiInput :: !(Arr Double)
  , jacobiCurrent :: !(Arr Double)
  , jacobiScratch :: !(Arr Double)
  }

data NBodyEnv = NBodyEnv
  { nbodyCount :: !Int
  , nbodyPosX :: !(Arr Double)
  , nbodyPosY :: !(Arr Double)
  , nbodyPosZ :: !(Arr Double)
  , nbodyMass :: !(Arr Double)
  , nbodyAccX :: !(Arr Double)
  , nbodyAccY :: !(Arr Double)
  , nbodyAccZ :: !(Arr Double)
  }

data DoubleMatrixEnv = DoubleMatrixEnv
  { doubleMatrixDim :: !Int
  , doubleMatrixLeft :: !(Arr Double)
  , doubleMatrixRight :: !(Arr Double)
  , doubleMatrixOutput :: !(Arr Double)
  }

data Int32MatrixEnv = Int32MatrixEnv
  { int32MatrixDim :: !Int
  , int32MatrixLeft :: !(Arr Int32)
  , int32MatrixRight :: !(Arr Int32)
  , int32MatrixOutput :: !(Arr Int32)
  }

data EditDistanceEnv = EditDistanceEnv
  { editRows :: !Int
  , editCols :: !Int
  , editLeft :: !(Arr Int)
  , editRight :: !(Arr Int)
  , editDp :: !(Arr Int)
  }

data LCSEnv = LCSEnv
  { lcsRows :: !Int
  , lcsCols :: !Int
  , lcsLeft :: !(Arr Int)
  , lcsRight :: !(Arr Int)
  , lcsDp :: !(Arr Int)
  }

data Jacobi2DEnv = Jacobi2DEnv
  { jacobi2DLength :: !Int
  , jacobi2DSteps :: !Int
  , jacobi2DInput :: !(Arr Double)
  , jacobi2DCurrent :: !(Arr Double)
  , jacobi2DScratch :: !(Arr Double)
  }

data Volume3DMapEnv = Volume3DMapEnv
  { volume3DLength :: !Int
  , volume3DLeft :: !(Arr Int)
  , volume3DRight :: !(Arr Int)
  , volume3DOutput :: !(Arr Int)
  }

data Force3 =
  Force3
    {-# UNPACK #-} !Double
    {-# UNPACK #-} !Double
    {-# UNPACK #-} !Double

benchmarks :: [Benchmark]
benchmarks =
  [ Benchmark "fill" "parallel 1D fill into a fresh array" 1000000 setupFill benchmarkPrepareNone runFillBenchmark validateFillBenchmark
  , Benchmark "fill-3d" "parallel 3D fill into a fresh volume" 128 setupFill3D benchmarkPrepareNone runFill3DBenchmark validateFill3DBenchmark
  , Benchmark "map" "parallel 1D elementwise map over two inputs" 1000000 setupBinaryVector benchmarkPrepareNone runMapBenchmark validateMapBenchmark
  , Benchmark "sum" "parallel reduction over one vector" 1000000 setupUnaryVector benchmarkPrepareNone runSumBenchmark validateSumBenchmark
  , Benchmark "dot" "parallel dot product over two vectors" 1000000 setupBinaryVector benchmarkPrepareNone runDotBenchmark validateDotBenchmark
  , Benchmark "jacobi-1d" "vectorized 1D Jacobi / heat-diffusion timesteps with double buffering" 262144 setupJacobi1D benchmarkPrepareJacobi runJacobi1DBenchmark validateJacobi1DBenchmark
  , Benchmark "verified-jacobi-1d" "vectorized 1D Jacobi via Loom.Verify with checked shifted accesses" 262144 setupJacobi1D benchmarkPrepareJacobi runVerifiedJacobi1DBenchmark validateVerifiedJacobi1DBenchmark
  , Benchmark "nbody" "parallel softened all-pairs n-body force accumulation with SoA layout and source tiling" 2048 setupNBody benchmarkPrepareNone runNBodyBenchmark validateNBodyBenchmark
  , Benchmark "matmul" "parallel square matrix multiply" 256 setupMatrix benchmarkPrepareNone runMatMulBenchmark validateMatMulBenchmark
  , Benchmark "tiled-matmul" "parallel square matrix multiply with tiled traversal" 256 setupMatrix benchmarkPrepareNone runTiledMatMulBenchmark validateTiledMatMulBenchmark
  , Benchmark "int32-tiled-matmul-scalar" "parallel square Int32 matrix multiply with tiling and scalar inner loops" 256 setupInt32Matrix benchmarkPrepareNone runInt32TiledMatMulScalarBenchmark validateInt32TiledMatMulScalarBenchmark
  , Benchmark "int32-tiled-matmul-vec" "parallel square Int32 matrix multiply with tiling and SIMD vectorization" 256 setupInt32Matrix benchmarkPrepareNone runInt32TiledMatMulVectorizedBenchmark validateInt32TiledMatMulVectorizedBenchmark
  , Benchmark "double-matmul-scalar" "parallel square double matrix multiply without tiling or vectorization" 256 setupDoubleMatrix benchmarkPrepareNone runDoubleMatMulScalarBenchmark validateDoubleMatMulScalarBenchmark
  , Benchmark "double-tiled-matmul-scalar" "parallel square double matrix multiply with tiling and scalar inner loops" 256 setupDoubleMatrix benchmarkPrepareNone runDoubleTiledMatMulScalarBenchmark validateDoubleTiledMatMulScalarBenchmark
  , Benchmark "double-tiled-matmul-vec" "parallel square double matrix multiply with tiling and SIMD vectorization" 256 setupDoubleMatrix benchmarkPrepareNone runDoubleTiledMatMulVectorizedBenchmark validateDoubleTiledMatMulVectorizedBenchmark
  , Benchmark "double-tiled-matmul-vec-newapi" "parallel square double matrix multiply with tiling and SIMD vectorization via shape-first Loom API" 256 setupDoubleMatrix benchmarkPrepareNone runDoubleTiledMatMulVectorizedNewApiBenchmark validateDoubleTiledMatMulVectorizedNewApiBenchmark
  , Benchmark "wavefront-edit-distance" "wavefront dynamic-programming edit distance" 1024 setupEditDistance benchmarkPrepareNone runWavefrontEditDistanceBenchmark validateWavefrontEditDistanceBenchmark
  , Benchmark "red-black-stencil" "barriered in-place red/black 5-point stencil sweep" 1024 setupImage benchmarkPrepareNone runRedBlackStencilBenchmark validateRedBlackStencilBenchmark
  , Benchmark "normalize-3phase" "three-phase normalization with two reductions and barriers" 1000000 setupDoubleVector benchmarkPrepareNone runThreePhaseNormalizeBenchmark validateThreePhaseNormalizeBenchmark
  , Benchmark "separable-blur" "two-pass 2D blur with a barriered scratch handoff" 1024 setupImage benchmarkPrepareNone runSeparableBlurBenchmark validateSeparableBlurBenchmark
  , Benchmark "wavefront-lcs" "wavefront longest-common-subsequence DP" 1024 setupLCS benchmarkPrepareNone runWavefrontLCSBenchmark validateWavefrontLCSBenchmark
  , Benchmark "jacobi-2d" "double-buffered 2D Jacobi heat diffusion, 50 steps" 512 setupJacobi2D benchmarkPrepareJacobi2D runJacobi2DBenchmark validateJacobi2DBenchmark
  , Benchmark "map-3d" "flat 3D elementwise map using parFor3" 128 setupVolume3DMap benchmarkPrepareNone runTiled3DMapBenchmark validateTiled3DMapBenchmark
  ]

lookupBenchmark :: String -> Maybe Benchmark
lookupBenchmark target = go benchmarks
  where
    go [] = Nothing
    go (benchmark : rest)
      | benchmarkName benchmark == target = Just benchmark
      | otherwise = go rest

benchmarkPrepareNone :: env -> IO ()
benchmarkPrepareNone = const (pure ())

setupFill :: Int -> IO FillEnv
setupFill n = do
  out <- newArr n
  pure (FillEnv n out)

setupFill3D :: Int -> IO Fill3DEnv
setupFill3D n = do
  out <- newArr (n * n * n)
  pure (Fill3DEnv n out)

setupUnaryVector :: Int -> IO UnaryEnv
setupUnaryVector n = do
  xs <- seededVector n 17 11
  pure (UnaryEnv n xs)

setupBinaryVector :: Int -> IO BinaryEnv
setupBinaryVector n = do
  xs <- seededVector n 17 11
  ys <- seededVector n 31 7
  out <- newArr n
  pure (BinaryEnv n xs ys out)

setupMatrix :: Int -> IO MatrixEnv
setupMatrix n = do
  a <- seededVector (n * n) 17 11
  b <- seededVector (n * n) 31 7
  out <- newArr (n * n)
  pure (MatrixEnv n a b out)

setupImage :: Int -> IO ImageEnv
setupImage n = do
  img <- seededVector (n * n) 17 11
  out <- newArr (n * n)
  scratch <- newArr (n * n)
  pure (ImageEnv n img out scratch)

setupDoubleVector :: Int -> IO DoubleUnaryEnv
setupDoubleVector n = do
  xs <- seededDoubleVector n 17 11
  out <- newArr n
  pure (DoubleUnaryEnv n xs out)

setupJacobi1D :: Int -> IO JacobiEnv
setupJacobi1D n = do
  xs <- seededDoubleVector n 17 11
  current <- seededDoubleVector n 17 11
  scratch <- newArr n
  pure (JacobiEnv n jacobiBenchmarkSteps xs current scratch)

setupNBody :: Int -> IO NBodyEnv
setupNBody n = do
  posX <- seededSignedDoubleVector n 17 11
  posY <- seededSignedDoubleVector n 31 7
  posZ <- seededSignedDoubleVector n 43 3
  mass <- seededPositiveDoubleVector n 29 5
  accX <- newArr n
  accY <- newArr n
  accZ <- newArr n
  pure (NBodyEnv n posX posY posZ mass accX accY accZ)

setupDoubleMatrix :: Int -> IO DoubleMatrixEnv
setupDoubleMatrix n = do
  a <- seededDoubleVector (n * n) 17 11
  b <- seededDoubleVector (n * n) 31 7
  out <- newArr (n * n)
  pure (DoubleMatrixEnv n a b out)

setupInt32Matrix :: Int -> IO Int32MatrixEnv
setupInt32Matrix n = do
  a <- seededInt32Vector (n * n) 17 11
  b <- seededInt32Vector (n * n) 31 7
  out <- newArr (n * n)
  pure (Int32MatrixEnv n a b out)

setupEditDistance :: Int -> IO EditDistanceEnv
setupEditDistance n = do
  left <- seededVector n 17 11
  right <- seededVector n 31 7
  dp <- newArr ((n + 1) * (n + 1))
  pure (EditDistanceEnv n n left right dp)

setupLCS :: Int -> IO LCSEnv
setupLCS n = do
  left <- seededVector n 17 11
  right <- seededVector n 31 7
  dp <- newArr ((n + 1) * (n + 1))
  pure (LCSEnv n n left right dp)

setupJacobi2D :: Int -> IO Jacobi2DEnv
setupJacobi2D n = do
  input <- seededDoubleVector (n * n) 17 11
  current <- seededDoubleVector (n * n) 17 11
  scratch <- newArr (n * n)
  pure (Jacobi2DEnv n jacobi2DBenchmarkSteps input current scratch)

setupVolume3DMap :: Int -> IO Volume3DMapEnv
setupVolume3DMap n = do
  left <- seededVector (n * n * n) 17 11
  right <- seededVector (n * n * n) 31 7
  out <- newArr (n * n * n)
  pure (Volume3DMapEnv n left right out)

seededVector :: Int -> Int -> Int -> IO (Arr Int)
seededVector n stride offset =
  fromList
    [ ((i * stride) + offset) `mod` 257
    | i <- [0 .. n - 1]
    ]

seededDoubleVector :: Int -> Int -> Int -> IO (Arr Double)
seededDoubleVector n stride offset =
  fromList
    [ fromIntegral (((i * stride) + offset) `mod` 257) / 257.0
    | i <- [0 .. n - 1]
    ]

seededSignedDoubleVector :: Int -> Int -> Int -> IO (Arr Double)
seededSignedDoubleVector n stride offset =
  fromList
    [ (2.0 * unitSeed i) - 1.0
    | i <- [0 .. n - 1]
    ]
  where
    unitSeed i =
      fromIntegral (((i * stride) + offset) `mod` 1021) / 1021.0

seededPositiveDoubleVector :: Int -> Int -> Int -> IO (Arr Double)
seededPositiveDoubleVector n stride offset =
  fromList
    [ 0.5 + unitSeed i
    | i <- [0 .. n - 1]
    ]
  where
    unitSeed i =
      fromIntegral (((i * stride) + offset) `mod` 1021) / 1021.0

seededInt32Vector :: Int -> Int -> Int -> IO (Arr Int32)
seededInt32Vector n stride offset =
  fromList
    [ fromIntegral (((i * stride) + offset) `mod` 257)
    | i <- [0 .. n - 1]
    ]

benchmarkPrepareJacobi :: JacobiEnv -> IO ()
benchmarkPrepareJacobi env =
  copyDoubleArrayKernel (jacobiLength env) (jacobiInput env) (jacobiCurrent env)

benchmarkPrepareJacobi2D :: Jacobi2DEnv -> IO ()
benchmarkPrepareJacobi2D env = do
  let n2 = jacobi2DLength env * jacobi2DLength env
  copyDoubleArrayKernel n2 (jacobi2DInput env) (jacobi2DCurrent env)
  copyDoubleArrayKernel n2 (jacobi2DInput env) (jacobi2DScratch env)

runFillBenchmark :: FillEnv -> IO BenchmarkRunResult
runFillBenchmark env =
  BenchmarkNoChecksum
    <$ runProg
      ( parallel $
          for1 (fillLength env) Schedule.identity $ \i ->
            writeArr (fillOutput env) i (i * 3 + 1)
      )

validateFillBenchmark :: FillEnv -> BenchmarkRunResult -> IO Int
validateFillBenchmark env BenchmarkNoChecksum =
  sampleVector (fillOutput env) (fillLength env)
validateFillBenchmark _ (BenchmarkChecksum _) =
  error "fill benchmark returned an unexpected checksum"

runFill3DBenchmark :: Fill3DEnv -> IO BenchmarkRunResult
runFill3DBenchmark env =
  BenchmarkNoChecksum
    <$ runProg
      ( parallel $
          for3 n n n Schedule.outerParallel $ \i j k ->
            writeArr
              (fill3DOutput env)
              ((i * n * n) + (j * n) + k)
              (i * 1000000 + j * 1000 + k)
      )
  where
    n = fill3DLength env

validateFill3DBenchmark :: Fill3DEnv -> BenchmarkRunResult -> IO Int
validateFill3DBenchmark env BenchmarkNoChecksum =
  sampleVolume (fill3DOutput env) (fill3DLength env)
validateFill3DBenchmark _ (BenchmarkChecksum _) =
  error "fill-3d benchmark returned an unexpected checksum"

runMapBenchmark :: BinaryEnv -> IO BenchmarkRunResult
runMapBenchmark env =
  BenchmarkNoChecksum
    <$ runProg
      ( parallel $
          for1 (binaryLength env) Schedule.identity $ \i -> do
            x <- readArr (binaryLeft env) i
            y <- readArr (binaryRight env) i
            writeArr (binaryOutput env) i (x + (2 * y))
      )

validateMapBenchmark :: BinaryEnv -> BenchmarkRunResult -> IO Int
validateMapBenchmark env BenchmarkNoChecksum =
  sampleVector (binaryOutput env) (binaryLength env)
validateMapBenchmark _ (BenchmarkChecksum _) =
  error "map benchmark returned an unexpected checksum"

runSumBenchmark :: UnaryEnv -> IO BenchmarkRunResult
runSumBenchmark env =
  BenchmarkChecksum
    <$> runProg
      ( parallel $
          parFoldFor intSum (unaryLength env) $ \i ->
            readArr (unaryInput env) i
      )

validateSumBenchmark :: UnaryEnv -> BenchmarkRunResult -> IO Int
validateSumBenchmark _ (BenchmarkChecksum checksum) = pure checksum
validateSumBenchmark _ BenchmarkNoChecksum =
  error "sum benchmark did not return a checksum"

runDotBenchmark :: BinaryEnv -> IO BenchmarkRunResult
runDotBenchmark env =
  BenchmarkChecksum
    <$> runProg
      ( parallel $
          parFoldFor intSum (binaryLength env) $ \i -> do
            x <- readArr (binaryLeft env) i
            y <- readArr (binaryRight env) i
            pure (x * y)
      )

validateDotBenchmark :: BinaryEnv -> BenchmarkRunResult -> IO Int
validateDotBenchmark _ (BenchmarkChecksum checksum) = pure checksum
validateDotBenchmark _ BenchmarkNoChecksum =
  error "dot benchmark did not return a checksum"

runJacobi1DBenchmark :: JacobiEnv -> IO BenchmarkRunResult
runJacobi1DBenchmark env = do
  go (jacobiSteps env) (jacobiCurrent env) (jacobiScratch env)
  pure BenchmarkNoChecksum
  where
    n = jacobiLength env
    go !remaining current next
      | remaining <= 0 = pure ()
      | otherwise = do
          runJacobi1DStepKernel n current next
          go (remaining - 1) next current

validateJacobi1DBenchmark :: JacobiEnv -> BenchmarkRunResult -> IO Int
validateJacobi1DBenchmark env BenchmarkNoChecksum =
  sampleDoubleVector finalBuffer (jacobiLength env)
  where
    finalBuffer
      | even (jacobiSteps env) = jacobiCurrent env
      | otherwise = jacobiScratch env
validateJacobi1DBenchmark _ (BenchmarkChecksum _) =
  error "jacobi benchmark returned an unexpected checksum"

runVerifiedJacobi1DBenchmark :: JacobiEnv -> IO BenchmarkRunResult
runVerifiedJacobi1DBenchmark env = do
  let n = jacobiLength env
      verifiedShape = Verify.shape1 n
      current = Verify.wrapArray verifiedShape (jacobiCurrent env)
      scratch = Verify.wrapArray verifiedShape (jacobiScratch env)
      go !remaining current' next
        | remaining <= 0 = pure ()
        | otherwise = do
            runVerifiedJacobi1DStepKernel n current' next
            go (remaining - 1) next current'
  go (jacobiSteps env) current scratch
  pure BenchmarkNoChecksum

validateVerifiedJacobi1DBenchmark :: JacobiEnv -> BenchmarkRunResult -> IO Int
validateVerifiedJacobi1DBenchmark env BenchmarkNoChecksum =
  sampleDoubleVector finalBuffer (jacobiLength env)
  where
    finalBuffer
      | even (jacobiSteps env) = jacobiCurrent env
      | otherwise = jacobiScratch env
validateVerifiedJacobi1DBenchmark _ (BenchmarkChecksum _) =
  error "verified jacobi benchmark returned an unexpected checksum"

runNBodyBenchmark :: NBodyEnv -> IO BenchmarkRunResult
runNBodyBenchmark env =
  BenchmarkNoChecksum
    <$ runNBodyKernel
      (nbodyCount env)
      (nbodyPosX env)
      (nbodyPosY env)
      (nbodyPosZ env)
      (nbodyMass env)
      (nbodyAccX env)
      (nbodyAccY env)
      (nbodyAccZ env)

validateNBodyBenchmark :: NBodyEnv -> BenchmarkRunResult -> IO Int
validateNBodyBenchmark env BenchmarkNoChecksum =
  sampleDoubleTriples (nbodyAccX env) (nbodyAccY env) (nbodyAccZ env) (nbodyCount env)
validateNBodyBenchmark _ (BenchmarkChecksum _) =
  error "nbody benchmark returned an unexpected checksum"

runMatMulBenchmark :: MatrixEnv -> IO BenchmarkRunResult
runMatMulBenchmark env =
  BenchmarkNoChecksum
    <$ runMatMulKernel (matrixDim env) (matrixLeft env) (matrixRight env) (matrixOutput env)

validateMatMulBenchmark :: MatrixEnv -> BenchmarkRunResult -> IO Int
validateMatMulBenchmark env BenchmarkNoChecksum =
  sampleMatrix (matrixOutput env) (matrixDim env)
validateMatMulBenchmark _ (BenchmarkChecksum _) =
  error "matmul benchmark returned an unexpected checksum"

runTiledMatMulBenchmark :: MatrixEnv -> IO BenchmarkRunResult
runTiledMatMulBenchmark env =
  BenchmarkNoChecksum
    <$ runTiledMatMulKernel (matrixDim env) (matrixLeft env) (matrixRight env) (matrixOutput env)

validateTiledMatMulBenchmark :: MatrixEnv -> BenchmarkRunResult -> IO Int
validateTiledMatMulBenchmark env BenchmarkNoChecksum =
  sampleMatrix (matrixOutput env) (matrixDim env)
validateTiledMatMulBenchmark _ (BenchmarkChecksum _) =
  error "tiled-matmul benchmark returned an unexpected checksum"

runInt32TiledMatMulScalarBenchmark :: Int32MatrixEnv -> IO BenchmarkRunResult
runInt32TiledMatMulScalarBenchmark env =
  BenchmarkNoChecksum
    <$ runInt32TiledMatMulScalarKernel
      (int32MatrixDim env)
      (int32MatrixLeft env)
      (int32MatrixRight env)
      (int32MatrixOutput env)

validateInt32TiledMatMulScalarBenchmark :: Int32MatrixEnv -> BenchmarkRunResult -> IO Int
validateInt32TiledMatMulScalarBenchmark env BenchmarkNoChecksum =
  sampleInt32Matrix (int32MatrixOutput env) (int32MatrixDim env)
validateInt32TiledMatMulScalarBenchmark _ (BenchmarkChecksum _) =
  error "int32-tiled-matmul-scalar benchmark returned an unexpected checksum"

runInt32TiledMatMulVectorizedBenchmark :: Int32MatrixEnv -> IO BenchmarkRunResult
runInt32TiledMatMulVectorizedBenchmark env =
  BenchmarkNoChecksum
    <$ runInt32TiledMatMulVectorizedKernel
      (int32MatrixDim env)
      (int32MatrixLeft env)
      (int32MatrixRight env)
      (int32MatrixOutput env)

validateInt32TiledMatMulVectorizedBenchmark :: Int32MatrixEnv -> BenchmarkRunResult -> IO Int
validateInt32TiledMatMulVectorizedBenchmark env BenchmarkNoChecksum =
  sampleInt32Matrix (int32MatrixOutput env) (int32MatrixDim env)
validateInt32TiledMatMulVectorizedBenchmark _ (BenchmarkChecksum _) =
  error "int32-tiled-matmul-vec benchmark returned an unexpected checksum"

runDoubleMatMulScalarBenchmark :: DoubleMatrixEnv -> IO BenchmarkRunResult
runDoubleMatMulScalarBenchmark env =
  BenchmarkNoChecksum
    <$ runDoubleMatMulScalarKernel
      (doubleMatrixDim env)
      (doubleMatrixLeft env)
      (doubleMatrixRight env)
      (doubleMatrixOutput env)

validateDoubleMatMulScalarBenchmark :: DoubleMatrixEnv -> BenchmarkRunResult -> IO Int
validateDoubleMatMulScalarBenchmark env BenchmarkNoChecksum =
  sampleDoubleMatrix (doubleMatrixOutput env) (doubleMatrixDim env)
validateDoubleMatMulScalarBenchmark _ (BenchmarkChecksum _) =
  error "double-matmul-scalar benchmark returned an unexpected checksum"

runDoubleTiledMatMulScalarBenchmark :: DoubleMatrixEnv -> IO BenchmarkRunResult
runDoubleTiledMatMulScalarBenchmark env =
  BenchmarkNoChecksum
    <$ runDoubleTiledMatMulScalarKernel
      (doubleMatrixDim env)
      (doubleMatrixLeft env)
      (doubleMatrixRight env)
      (doubleMatrixOutput env)

validateDoubleTiledMatMulScalarBenchmark :: DoubleMatrixEnv -> BenchmarkRunResult -> IO Int
validateDoubleTiledMatMulScalarBenchmark env BenchmarkNoChecksum =
  sampleDoubleMatrix (doubleMatrixOutput env) (doubleMatrixDim env)
validateDoubleTiledMatMulScalarBenchmark _ (BenchmarkChecksum _) =
  error "double-tiled-matmul-scalar benchmark returned an unexpected checksum"

runDoubleTiledMatMulVectorizedBenchmark :: DoubleMatrixEnv -> IO BenchmarkRunResult
runDoubleTiledMatMulVectorizedBenchmark env =
  BenchmarkNoChecksum
    <$ runDoubleTiledMatMulVectorizedKernel
      (doubleMatrixDim env)
      (doubleMatrixLeft env)
      (doubleMatrixRight env)
      (doubleMatrixOutput env)

validateDoubleTiledMatMulVectorizedBenchmark :: DoubleMatrixEnv -> BenchmarkRunResult -> IO Int
validateDoubleTiledMatMulVectorizedBenchmark env BenchmarkNoChecksum =
  sampleDoubleMatrix (doubleMatrixOutput env) (doubleMatrixDim env)
validateDoubleTiledMatMulVectorizedBenchmark _ (BenchmarkChecksum _) =
  error "double-tiled-matmul-vec benchmark returned an unexpected checksum"

runDoubleTiledMatMulVectorizedNewApiBenchmark :: DoubleMatrixEnv -> IO BenchmarkRunResult
runDoubleTiledMatMulVectorizedNewApiBenchmark env =
  BenchmarkNoChecksum
    <$ runDoubleTiledMatMulVectorizedNewApiKernel
      (doubleMatrixDim env)
      (doubleMatrixLeft env)
      (doubleMatrixRight env)
      (doubleMatrixOutput env)

validateDoubleTiledMatMulVectorizedNewApiBenchmark :: DoubleMatrixEnv -> BenchmarkRunResult -> IO Int
validateDoubleTiledMatMulVectorizedNewApiBenchmark env BenchmarkNoChecksum =
  sampleDoubleMatrix (doubleMatrixOutput env) (doubleMatrixDim env)
validateDoubleTiledMatMulVectorizedNewApiBenchmark _ (BenchmarkChecksum _) =
  error "double-tiled-matmul-vec-newapi benchmark returned an unexpected checksum"

runWavefrontEditDistanceBenchmark :: EditDistanceEnv -> IO BenchmarkRunResult
runWavefrontEditDistanceBenchmark env =
  BenchmarkNoChecksum
    <$ runWavefrontEditDistanceKernel
      (editRows env)
      (editCols env)
      (editLeft env)
      (editRight env)
      (editDp env)

validateWavefrontEditDistanceBenchmark :: EditDistanceEnv -> BenchmarkRunResult -> IO Int
validateWavefrontEditDistanceBenchmark env BenchmarkNoChecksum =
  sampleMatrix (editDp env) (editCols env + 1)
validateWavefrontEditDistanceBenchmark _ (BenchmarkChecksum _) =
  error "wavefront-edit-distance benchmark returned an unexpected checksum"

runRedBlackStencilBenchmark :: ImageEnv -> IO BenchmarkRunResult
runRedBlackStencilBenchmark env =
  BenchmarkNoChecksum
    <$ runRedBlackStencilKernel (imageDim env) (imageInput env) (imageOutput env)

validateRedBlackStencilBenchmark :: ImageEnv -> BenchmarkRunResult -> IO Int
validateRedBlackStencilBenchmark env BenchmarkNoChecksum =
  sampleMatrix (imageOutput env) (imageDim env)
validateRedBlackStencilBenchmark _ (BenchmarkChecksum _) =
  error "red-black-stencil benchmark returned an unexpected checksum"

runThreePhaseNormalizeBenchmark :: DoubleUnaryEnv -> IO BenchmarkRunResult
runThreePhaseNormalizeBenchmark env =
  BenchmarkNoChecksum
    <$ runThreePhaseNormalizeKernel (doubleLength env) (doubleInput env) (doubleOutput env)

validateThreePhaseNormalizeBenchmark :: DoubleUnaryEnv -> BenchmarkRunResult -> IO Int
validateThreePhaseNormalizeBenchmark env BenchmarkNoChecksum =
  sampleDoubleVector (doubleOutput env) (doubleLength env)
validateThreePhaseNormalizeBenchmark _ (BenchmarkChecksum _) =
  error "normalize-3phase benchmark returned an unexpected checksum"

runSeparableBlurBenchmark :: ImageEnv -> IO BenchmarkRunResult
runSeparableBlurBenchmark env =
  BenchmarkNoChecksum
    <$ runSeparableBlurKernelWithScratch
      (imageDim env)
      (imageInput env)
      (imageScratch env)
      (imageOutput env)

validateSeparableBlurBenchmark :: ImageEnv -> BenchmarkRunResult -> IO Int
validateSeparableBlurBenchmark env BenchmarkNoChecksum =
  sampleMatrix (imageOutput env) (imageDim env)
validateSeparableBlurBenchmark _ (BenchmarkChecksum _) =
  error "separable-blur benchmark returned an unexpected checksum"

runWavefrontLCSBenchmark :: LCSEnv -> IO BenchmarkRunResult
runWavefrontLCSBenchmark env =
  BenchmarkNoChecksum
    <$ runWavefrontLCSKernel
      (lcsRows env)
      (lcsCols env)
      (lcsLeft env)
      (lcsRight env)
      (lcsDp env)

validateWavefrontLCSBenchmark :: LCSEnv -> BenchmarkRunResult -> IO Int
validateWavefrontLCSBenchmark env BenchmarkNoChecksum =
  sampleMatrix (lcsDp env) (lcsCols env + 1)
validateWavefrontLCSBenchmark _ (BenchmarkChecksum _) =
  error "wavefront-lcs benchmark returned an unexpected checksum"

runJacobi2DBenchmark :: Jacobi2DEnv -> IO BenchmarkRunResult
runJacobi2DBenchmark env = do
  go (jacobi2DSteps env) (jacobi2DCurrent env) (jacobi2DScratch env)
  pure BenchmarkNoChecksum
  where
    n = jacobi2DLength env
    go !remaining !cur !nxt
      | remaining <= 0 = pure ()
      | otherwise = do
          runJacobi2DStepKernel n cur nxt
          go (remaining - 1) nxt cur

validateJacobi2DBenchmark :: Jacobi2DEnv -> BenchmarkRunResult -> IO Int
validateJacobi2DBenchmark env BenchmarkNoChecksum =
  sampleDoubleMatrix finalBuffer (jacobi2DLength env)
  where
    finalBuffer
      | even (jacobi2DSteps env) = jacobi2DCurrent env
      | otherwise                = jacobi2DScratch env
validateJacobi2DBenchmark _ (BenchmarkChecksum _) =
  error "jacobi-2d benchmark returned an unexpected checksum"

runTiled3DMapBenchmark :: Volume3DMapEnv -> IO BenchmarkRunResult
runTiled3DMapBenchmark env =
  BenchmarkNoChecksum
    <$ runTiled3DMapKernel
      (volume3DLength env)
      (volume3DLeft env)
      (volume3DRight env)
      (volume3DOutput env)

validateTiled3DMapBenchmark :: Volume3DMapEnv -> BenchmarkRunResult -> IO Int
validateTiled3DMapBenchmark env BenchmarkNoChecksum =
  sampleVolume (volume3DOutput env) (volume3DLength env)
validateTiled3DMapBenchmark _ (BenchmarkChecksum _) =
  error "tiled-3d-map benchmark returned an unexpected checksum"

runFill :: Int -> IO Int
runFill n = do
  arr <- newArr n
  runProg $
    parallel $
      for1 n Schedule.identity $ \i ->
        writeArr arr i (i * 3 + 1)
  sampleVector arr n

runFill3D :: Int -> IO Int
runFill3D n = do
  arr <- newArr (n * n * n)
  runProg $
    parallel $
      for3 n n n Schedule.identity $ \i j k ->
        writeArr arr ((i * n * n) + (j * n) + k) (i * 1000000 + j * 1000 + k)
  sampleVolume arr n

runMap :: BinaryEnv -> IO Int
runMap env = do
  let n = binaryLength env
  out <- newArr n
  runProg $
    parallel $
      for1 n Schedule.identity $ \i -> do
        x <- readArr (binaryLeft env) i
        y <- readArr (binaryRight env) i
        writeArr out i (x + (2 * y))
  sampleVector out n

runSum :: UnaryEnv -> IO Int
runSum env =
  runProg $
    parallel $
      newReducer intSum $ \sumVar -> do
        for1 (unaryLength env) Schedule.identity $ \i -> do
          x <- readArr (unaryInput env) i
          reduce sumVar x
        getReducer sumVar

runDot :: BinaryEnv -> IO Int
runDot env =
  runProg $
    parallel $
      newReducer intSum $ \sumVar -> do
        for1 (binaryLength env) Schedule.identity $ \i -> do
          x <- readArr (binaryLeft env) i
          y <- readArr (binaryRight env) i
          reduce sumVar (x * y)
        getReducer sumVar

runJacobi1D :: JacobiEnv -> IO Int
runJacobi1D env = do
  out <- runJacobi1DKernel (jacobiSteps env) (jacobiLength env) (jacobiInput env)
  sampleDoubleVector out (jacobiLength env)

runVerifiedJacobi1D :: JacobiEnv -> IO Int
runVerifiedJacobi1D env = do
  out <- runVerifiedJacobi1DKernel (jacobiSteps env) (jacobiLength env) (jacobiInput env)
  sampleDoubleVector out (jacobiLength env)

runNBody :: NBodyEnv -> IO Int
runNBody env = do
  let n = nbodyCount env
  accX <- newArr n
  accY <- newArr n
  accZ <- newArr n
  runNBodyKernel
    n
    (nbodyPosX env)
    (nbodyPosY env)
    (nbodyPosZ env)
    (nbodyMass env)
    accX
    accY
    accZ
  sampleDoubleTriples accX accY accZ n

runMatMul :: MatrixEnv -> IO Int
runMatMul env = do
  let n = matrixDim env
  out <- newArr (n * n)
  runMatMulKernel n (matrixLeft env) (matrixRight env) out
  sampleMatrix out n

runTiledMatMul :: MatrixEnv -> IO Int
runTiledMatMul env = do
  let n = matrixDim env
  out <- newArr (n * n)
  runTiledMatMulKernel n (matrixLeft env) (matrixRight env) out
  sampleMatrix out n

runInt32TiledMatMulScalar :: Int32MatrixEnv -> IO Int
runInt32TiledMatMulScalar env = do
  let n = int32MatrixDim env
  out <- newArr (n * n)
  runInt32TiledMatMulScalarKernel n (int32MatrixLeft env) (int32MatrixRight env) out
  sampleInt32Matrix out n

runInt32TiledMatMulVectorized :: Int32MatrixEnv -> IO Int
runInt32TiledMatMulVectorized env = do
  let n = int32MatrixDim env
  out <- newArr (n * n)
  runInt32TiledMatMulVectorizedKernel n (int32MatrixLeft env) (int32MatrixRight env) out
  sampleInt32Matrix out n

runDoubleMatMulScalar :: DoubleMatrixEnv -> IO Int
runDoubleMatMulScalar env = do
  let n = doubleMatrixDim env
  out <- newArr (n * n)
  runDoubleMatMulScalarKernel n (doubleMatrixLeft env) (doubleMatrixRight env) out
  sampleDoubleMatrix out n

runDoubleTiledMatMulScalar :: DoubleMatrixEnv -> IO Int
runDoubleTiledMatMulScalar env = do
  let n = doubleMatrixDim env
  out <- newArr (n * n)
  runDoubleTiledMatMulScalarKernel n (doubleMatrixLeft env) (doubleMatrixRight env) out
  sampleDoubleMatrix out n

runDoubleTiledMatMulVectorized :: DoubleMatrixEnv -> IO Int
runDoubleTiledMatMulVectorized env = do
  let n = doubleMatrixDim env
  out <- newArr (n * n)
  runDoubleTiledMatMulVectorizedKernel n (doubleMatrixLeft env) (doubleMatrixRight env) out
  sampleDoubleMatrix out n

runDoubleTiledMatMulVectorizedNewApi :: DoubleMatrixEnv -> IO Int
runDoubleTiledMatMulVectorizedNewApi env = do
  let n = doubleMatrixDim env
  out <- newArr (n * n)
  runDoubleTiledMatMulVectorizedNewApiKernel n (doubleMatrixLeft env) (doubleMatrixRight env) out
  sampleDoubleMatrix out n

runMatMulExample :: Int -> [Int] -> [Int] -> IO [Int]
runMatMulExample n xs ys = do
  left <- fromList xs
  right <- fromList ys
  out <- newArr (n * n)
  runMatMulKernel n left right out
  toList out

runInt32TiledMatMulScalarExample :: Int -> [Int32] -> [Int32] -> IO [Int32]
runInt32TiledMatMulScalarExample n xs ys = do
  left <- fromList xs
  right <- fromList ys
  out <- newArr (n * n)
  runInt32TiledMatMulScalarKernel n left right out
  toList out

runInt32TiledMatMulVectorizedExample :: Int -> [Int32] -> [Int32] -> IO [Int32]
runInt32TiledMatMulVectorizedExample n xs ys = do
  left <- fromList xs
  right <- fromList ys
  out <- newArr (n * n)
  runInt32TiledMatMulVectorizedKernel n left right out
  toList out

runDoubleMatMulExample :: Int -> [Double] -> [Double] -> IO [Double]
runDoubleMatMulExample n xs ys = do
  left <- fromList xs
  right <- fromList ys
  out <- newArr (n * n)
  runDoubleMatMulScalarKernel n left right out
  toList out

runDoubleTiledMatMulScalarExample :: Int -> [Double] -> [Double] -> IO [Double]
runDoubleTiledMatMulScalarExample n xs ys = do
  left <- fromList xs
  right <- fromList ys
  out <- newArr (n * n)
  runDoubleTiledMatMulScalarKernel n left right out
  toList out

runDoubleTiledMatMulVectorizedExample :: Int -> [Double] -> [Double] -> IO [Double]
runDoubleTiledMatMulVectorizedExample n xs ys = do
  left <- fromList xs
  right <- fromList ys
  out <- newArr (n * n)
  runDoubleTiledMatMulVectorizedKernel n left right out
  toList out

runDoubleTiledMatMulVectorizedNewApiExample :: Int -> [Double] -> [Double] -> IO [Double]
runDoubleTiledMatMulVectorizedNewApiExample n xs ys = do
  left <- fromList xs
  right <- fromList ys
  out <- newArr (n * n)
  runDoubleTiledMatMulVectorizedNewApiKernel n left right out
  toList out

runJacobi1DExample :: Int -> [Double] -> IO [Double]
runJacobi1DExample steps xs = do
  src <- fromList xs
  out <- runJacobi1DKernel steps (length xs) src
  toList out

runVerifiedJacobi1DExample :: Int -> [Double] -> IO [Double]
runVerifiedJacobi1DExample steps xs = do
  src <- fromList xs
  out <- runVerifiedJacobi1DKernel steps (length xs) src
  toList out

runNBodyExample :: [Double] -> [Double] -> [Double] -> [Double] -> IO [Double]
runNBodyExample posX posY posZ mass
  | length posY /= n || length posZ /= n || length mass /= n =
      error "runNBodyExample: mismatched input lengths"
  | otherwise = do
      xs <- fromList posX
      ys <- fromList posY
      zs <- fromList posZ
      ms <- fromList mass
      outX <- newArr n
      outY <- newArr n
      outZ <- newArr n
      runNBodyKernel n xs ys zs ms outX outY outZ
      interleave3 <$> toList outX <*> toList outY <*> toList outZ
  where
    n = length posX

runFill3DExample :: Int -> IO [Int]
runFill3DExample n = do
  arr <- newArr (n * n * n)
  runProg $
    parallel $
      for3 n n n Schedule.identity $ \i j k ->
        writeArr arr (i * n * n + j * n + k) (i * 100 + j * 10 + k)
  toList arr

runPolyhedralTiledMatMulExample :: Int -> [Int] -> [Int] -> IO [Int]
runPolyhedralTiledMatMulExample n xs ys = do
  left <- fromList xs
  right <- fromList ys
  out <- newArr (n * n)
  runProg $
    parallel $
      requirePolyhedral "polyhedral tiled matmul" (polyhedralTiledMatMulKernel n left right out)
  toList out

runWavefrontEditDistance :: EditDistanceEnv -> IO Int
runWavefrontEditDistance env = do
  let rows = editRows env
      cols = editCols env
      tableCols = cols + 1
  dp <- newArr ((rows + 1) * tableCols)
  runWavefrontEditDistanceKernel rows cols (editLeft env) (editRight env) dp
  sampleMatrix dp tableCols

runRedBlackStencil :: ImageEnv -> IO Int
runRedBlackStencil env = do
  let n = imageDim env
  out <- newArr (n * n)
  runRedBlackStencilKernel n (imageInput env) out
  sampleMatrix out n

runThreePhaseNormalize :: DoubleUnaryEnv -> IO Int
runThreePhaseNormalize env = do
  let n = doubleLength env
  out <- newArr n
  runThreePhaseNormalizeKernel n (doubleInput env) out
  sampleDoubleVector out n

runSeparableBlur :: ImageEnv -> IO Int
runSeparableBlur env = do
  let n = imageDim env
  out <- newArr (n * n)
  runSeparableBlurKernel n (imageInput env) out
  sampleMatrix out n

runRedBlackStencilExample :: Int -> [Int] -> IO [Int]
runRedBlackStencilExample n xs = do
  src <- fromList xs
  out <- newArr (n * n)
  runRedBlackStencilKernel n src out
  toList out

runThreePhaseNormalizeExample :: [Double] -> IO [Double]
runThreePhaseNormalizeExample xs = do
  src <- fromList xs
  out <- newArr (length xs)
  runThreePhaseNormalizeKernel (length xs) src out
  toList out

runWavefrontEditDistanceExample :: [Int] -> [Int] -> IO Int
runWavefrontEditDistanceExample xs ys = do
  left <- fromList xs
  right <- fromList ys
  let rows = length xs
      cols = length ys
      tableCols = cols + 1
  dp <- newArr ((rows + 1) * tableCols)
  runWavefrontEditDistanceKernel rows cols left right dp
  readArrIO dp (rows * tableCols + cols)

runPolyhedralWavefrontEditDistanceExample :: [Int] -> [Int] -> IO Int
runPolyhedralWavefrontEditDistanceExample xs ys = do
  left <- fromList xs
  right <- fromList ys
  let rows = length xs
      cols = length ys
      tableCols = cols + 1
  dp <- newArr ((rows + 1) * tableCols)
  runProg $
    parallel $ do
      for1 (rows + 1) Schedule.identity $ \i ->
        writeArr dp (i * tableCols) i
      barrier
      for1 cols Schedule.identity $ \j0 ->
        let j = j0 + 1
         in writeArr dp j j
      barrier
      requirePolyhedral
        "polyhedral wavefront edit distance"
        (polyhedralWavefrontKernel rows cols tableCols left right dp)
  readArrIO dp (rows * tableCols + cols)

runSeparableBlurExample :: Int -> [Int] -> IO [Int]
runSeparableBlurExample n xs = do
  src <- fromList xs
  out <- newArr (n * n)
  runSeparableBlurKernel n src out
  toList out

runMatMulKernel :: Int -> Arr Int -> Arr Int -> Arr Int -> IO ()
runMatMulKernel n left right out =
  runProg $
    parallel $
      for1 n Schedule.identity $ \i ->
        stripMine vecWidth n
          (\j0 -> writeMatMulVecChunk n left right out i j0)
          (\j -> writeMatMulScalarCell n left right out i j)

runTiledMatMulKernel :: Int -> Arr Int -> Arr Int -> Arr Int -> IO ()
runTiledMatMulKernel n left right out =
  runProg $
    parallel $
      tile2D tile tile (sh2 n n) $ \row0 col0 -> do
        let rowLimit = min n (row0 + tile)
            colLimit = min n (col0 + tile)
            rowCount = rowLimit - row0
            colCount = colLimit - col0
        parFor rowCount $ \i0 -> do
          let i = row0 + i0
          stripMine vecWidth colCount
            (\j0 -> writeMatMulVecChunk n left right out i (col0 + j0))
            (\j -> writeMatMulScalarCell n left right out i (col0 + j))
  where
    tile = max 1 (min 32 n)

runInt32TiledMatMulScalarKernel :: Int -> Arr Int32 -> Arr Int32 -> Arr Int32 -> IO ()
runInt32TiledMatMulScalarKernel n left right out =
  runProg $
    parallel $
      tile2D tile tile (sh2 n n) $ \row0 col0 -> do
        let rowLimit = min n (row0 + tile)
            colLimit = min n (col0 + tile)
            rowCount = rowLimit - row0
            colCount = colLimit - col0
        parFor rowCount $ \i0 -> do
          let i = row0 + i0
          parFor colCount $ \j0 ->
            writeMatMulScalarCellInt32 n left right out i (col0 + j0)
  where
    tile = max 1 (min 32 n)

runInt32TiledMatMulVectorizedKernel :: Int -> Arr Int32 -> Arr Int32 -> Arr Int32 -> IO ()
runInt32TiledMatMulVectorizedKernel n left right out =
  runProg $
    parallel $
      tile2D tile tile (sh2 n n) $ \row0 col0 -> do
        let rowLimit = min n (row0 + tile)
            colLimit = min n (col0 + tile)
            rowCount = rowLimit - row0
            colCount = colLimit - col0
        parFor rowCount $ \i0 -> do
          let i = row0 + i0
          stripMine vecWidth colCount
            (\j0 -> writeMatMulVecChunkInt32 n left right out i (col0 + j0))
            (\j -> writeMatMulScalarCellInt32 n left right out i (col0 + j))
  where
    tile = max 1 (min 32 n)

runDoubleMatMulScalarKernel :: Int -> Arr Double -> Arr Double -> Arr Double -> IO ()
runDoubleMatMulScalarKernel n left right out =
  runProg $
    parallel $
      for2 n n Schedule.outerParallel $ \i j ->
        writeMatMulScalarCellDouble n left right out i j

runDoubleTiledMatMulScalarKernel :: Int -> Arr Double -> Arr Double -> Arr Double -> IO ()
runDoubleTiledMatMulScalarKernel n left right out =
  runProg $
    parallel $
      tile2D tile tile (sh2 n n) $ \row0 col0 -> do
        let rowLimit = min n (row0 + tile)
            colLimit = min n (col0 + tile)
            rowCount = rowLimit - row0
            colCount = colLimit - col0
        parFor rowCount $ \i0 -> do
          let i = row0 + i0
          parFor colCount $ \j0 ->
            writeMatMulScalarCellDouble n left right out i (col0 + j0)
  where
    tile = max 1 (min 32 n)

runDoubleTiledMatMulVectorizedKernel :: Int -> Arr Double -> Arr Double -> Arr Double -> IO ()
runDoubleTiledMatMulVectorizedKernel n left right out =
  runProg $
    parallel $
      tile2D tile tile (sh2 n n) $ \row0 col0 -> do
        let rowLimit = min n (row0 + tile)
            colLimit = min n (col0 + tile)
            rowCount = rowLimit - row0
            colCount = colLimit - col0
        parFor rowCount $ \i0 -> do
          let i = row0 + i0
          stripMine vecWidth colCount
            (\j0 -> writeMatMulVecChunkDouble n left right out i (col0 + j0))
            (\j -> writeMatMulScalarCellDouble n left right out i (col0 + j))
  where
    tile = max 1 (min 32 n)

runDoubleTiledMatMulVectorizedNewApiKernel :: Int -> Arr Double -> Arr Double -> Arr Double -> IO ()
runDoubleTiledMatMulVectorizedNewApiKernel n left right out =
  runProg $
    parallel $ do
      for2 n chunkCount (Schedule.tile2 tile tileChunks) $ \i chunk ->
        writeMatMulVecChunkDouble n left right out i (chunk * vecWidth)
      if remainderCount > 0
        then
          for2 n remainderCount (Schedule.tile2 tile tile) $ \i jOffset ->
            writeMatMulScalarCellDouble n left right out i (remainderStart + jOffset)
        else pure ()
  where
    tile = max 1 (min 32 n)
    tileChunks = max 1 (tile `quot` vecWidth)
    chunkCount = n `quot` vecWidth
    remainderStart = chunkCount * vecWidth
    remainderCount = n - remainderStart

runJacobi1DKernel :: Int -> Int -> Arr Double -> IO (Arr Double)
runJacobi1DKernel steps n src = do
  current <- newArr n
  scratch <- newArr n
  copyDoubleArrayKernel n src current
  go steps current scratch
  where
    go !remaining current next
      | remaining <= 0 = pure current
      | otherwise = do
          runJacobi1DStepKernel n current next
          go (remaining - 1) next current

runVerifiedJacobi1DKernel :: Int -> Int -> Arr Double -> IO (Arr Double)
runVerifiedJacobi1DKernel steps n src = do
  let verifiedShape = Verify.shape1 n
      srcArray = Verify.wrapArray verifiedShape src
  current <- Verify.newArray verifiedShape
  scratch <- Verify.newArray verifiedShape
  copyVerifiedArray verifiedShape srcArray current
  go steps current scratch
  where
    go !remaining current next
      | remaining <= 0 = pure (Verify.unwrapArray current)
      | otherwise = do
          runVerifiedJacobi1DStepKernel n current next
          go (remaining - 1) next current

runJacobi1DStepKernel :: Int -> Arr Double -> Arr Double -> IO ()
runJacobi1DStepKernel n prev next =
  runProg $
    parallel $ do
      first <- readArr prev 0
      writeArr next 0 first
      if n > 1
        then do
          lastValue <- readArr prev (n - 1)
          writeArr next (n - 1) lastValue
        else pure ()
      let interiorCount = max 0 (n - 2)
      stripMine vecWidth interiorCount
        (\offset -> writeJacobi1DVecChunk prev next (offset + 1))
        (\offset -> writeJacobi1DScalarCell prev next (offset + 1))

runVerifiedJacobi1DStepKernel n prev next =
  let verifiedShape = Verify.shape1 n
  in Verify.runProg $
        Verify.parallel $ do
          let readCtx = Verify.rectReadAccess1D verifiedShape
              writeCtx = Verify.rectWriteAccess1D verifiedShape
              writeVecChunk ix = do
                left <- Verify.readDVecOffsetAt1D readCtx prev (-1) ix
                center <- Verify.readDVecAt1D readCtx prev ix
                right <- Verify.readDVecOffsetAt1D readCtx prev 1 ix
                let total = addDVec (addDVec left right) (addDVec center center)
                Verify.writeDVecAt1D writeCtx next ix (mulDVec total jacobiWeightVec)
              writeScalarCell ix = do
                left <- Verify.readOffsetAt1D readCtx prev (-1) ix
                center <- Verify.readAt readCtx prev ix
                right <- Verify.readOffsetAt1D readCtx prev 1 ix
                Verify.writeAt writeCtx next ix ((left + center + center + right) * 0.25)
          first <- Verify.readAt readCtx prev (Verify.rectIx1 0)
          Verify.writeAt writeCtx next (Verify.rectIx1 0) first
          if n > 1
            then do
              lastValue <- Verify.readAt readCtx prev (Verify.rectIx1 (n - 1))
              Verify.writeAt writeCtx next (Verify.rectIx1 (n - 1)) lastValue
            else pure ()
          let interiorCount = max 0 (n - 2)
          stripMine vecWidth interiorCount
            (\offset -> writeVecChunk (Verify.rectIx1 (offset + 1)))
            (\offset -> writeScalarCell (Verify.rectIx1 (offset + 1)))

copyVerifiedArray verifiedShape src dst =
  Verify.runProg $
    Verify.parallel $
      Verify.parFor1D verifiedShape $ \ctx ix -> do
        x <- Verify.readAt ctx src ix
        Verify.writeAt ctx dst ix x

copyDoubleArrayKernel :: Int -> Arr Double -> Arr Double -> IO ()
copyDoubleArrayKernel n src dst =
  runProg $
    parallel $
      for1 n Schedule.identity $ \i -> do
        x <- readArr src i
        writeArr dst i x

{-# INLINE writeJacobi1DVecChunk #-}
writeJacobi1DVecChunk :: Arr Double -> Arr Double -> Int -> Prog ()
writeJacobi1DVecChunk prev next i = do
  left <- readDVec prev (i - 1)
  center <- readDVec prev i
  right <- readDVec prev (i + 1)
  let total = addDVec (addDVec left right) (addDVec center center)
  writeDVec next i (mulDVec total jacobiWeightVec)

{-# INLINE writeJacobi1DScalarCell #-}
writeJacobi1DScalarCell :: Arr Double -> Arr Double -> Int -> Prog ()
writeJacobi1DScalarCell prev next i = do
  left <- readArr prev (i - 1)
  center <- readArr prev i
  right <- readArr prev (i + 1)
  writeArr next i ((left + center + center + right) * 0.25)

jacobiBenchmarkSteps :: Int
jacobiBenchmarkSteps = 50

{-# INLINE jacobiWeightVec #-}
jacobiWeightVec :: DVec
jacobiWeightVec = broadcastDVec 0.25

runNBodyKernel ::
  Int ->
  Arr Double ->
  Arr Double ->
  Arr Double ->
  Arr Double ->
  Arr Double ->
  Arr Double ->
  Arr Double ->
  IO ()
runNBodyKernel n posX posY posZ mass accX accY accZ =
  runProg $
    parallel $
      for1 n Schedule.identity $ \i -> do
        xi <- readArr posX i
        yi <- readArr posY i
        zi <- readArr posZ i
        Force3 ax ay az <-
          accumFor n zeroForce $ \acc j ->
            accumulateNBodyForce posX posY posZ mass xi yi zi acc j
        writeArr accX i ax
        writeArr accY i ay
        writeArr accZ i az

{-# INLINE zeroForce #-}
zeroForce :: Force3
zeroForce = Force3 0.0 0.0 0.0

{-# INLINE addForce3 #-}
addForce3 :: Force3 -> Force3 -> Force3
addForce3 (Force3 ax ay az) (Force3 bx by bz) =
  Force3 (ax + bx) (ay + by) (az + bz)

{-# INLINE accumulateNBodyForce #-}
accumulateNBodyForce ::
  Arr Double ->
  Arr Double ->
  Arr Double ->
  Arr Double ->
  Double ->
  Double ->
  Double ->
  Force3 ->
  Int ->
  Prog Force3
accumulateNBodyForce posX posY posZ mass xi yi zi (Force3 ax ay az) j = do
  xj <- readArr posX j
  yj <- readArr posY j
  zj <- readArr posZ j
  mj <- readArr mass j
  let dx = xj - xi
      dy = yj - yi
      dz = zj - zi
      distSq = (dx * dx) + (dy * dy) + (dz * dz) + nbodySoftening
      invDist = 1.0 / sqrt distSq
      scale = mj * invDist * invDist * invDist
  pure
    ( Force3
        (ax + (dx * scale))
        (ay + (dy * scale))
        (az + (dz * scale))
    )

-- | SIMD inner step: process 4 source particles at once using 'DVec'.
--
-- Reads a 4-wide chunk from each source array, computes per-lane
-- displacement, distance, and softened inverse-distance, then
-- horizontally sums the force contributions into the scalar accumulator.
-- The @xi4@\/@yi4@\/@zi4@\/@eps4@ arguments are pre-broadcast outside
-- the j-loop so they are not recomputed per iteration.
--
-- The per-lane @1\/sqrt@ is implemented via 'invSqrtDVec', which unpacks
-- each lane, applies 'sqrtDouble#', and repacks.  LLVM's SLP vectoriser
-- recognises this pattern and emits a vectorised sqrt instruction.
{-# INLINE accumulateNBodyForceSimd #-}
accumulateNBodyForceSimd ::
  Arr Double ->
  Arr Double ->
  Arr Double ->
  Arr Double ->
  DVec ->
  DVec ->
  DVec ->
  DVec ->
  Force3 ->
  Int ->
  Prog Force3
accumulateNBodyForceSimd posX posY posZ mass xi4 yi4 zi4 eps4 (Force3 ax ay az) j = do
  xj4 <- readDVec posX j
  yj4 <- readDVec posY j
  zj4 <- readDVec posZ j
  mj4 <- readDVec mass j
  let dx4 = subDVec xj4 xi4
      dy4 = subDVec yj4 yi4
      dz4 = subDVec zj4 zi4
      distSq4 =
        addDVec
          (addDVec (mulDVec dx4 dx4) (addDVec (mulDVec dy4 dy4) (mulDVec dz4 dz4)))
          eps4
      invDist4 = invSqrtDVec distSq4
      invDist3_4 = mulDVec invDist4 (mulDVec invDist4 invDist4)
      scale4 = mulDVec mj4 invDist3_4
  pure
    ( Force3
        (ax + sumDVec (mulDVec dx4 scale4))
        (ay + sumDVec (mulDVec dy4 scale4))
        (az + sumDVec (mulDVec dz4 scale4))
    )

nbodySoftening :: Double
nbodySoftening = 1.0e-9

{-# INLINE writeMatMulVecChunk #-}
writeMatMulVecChunk :: Int -> Arr Int -> Arr Int -> Arr Int -> Int -> Int -> Prog ()
writeMatMulVecChunk n left right out i j0 = do
  total <- accumIVecFor n (broadcastIVec 0) $ \acc k -> do
    lhs <- readArr left (i * n + k)
    rhs <- readIVec right (k * n + j0)
    pure (addIVec acc (mulIVec (broadcastIVec lhs) rhs))
  writeIVec out (i * n + j0) total

{-# INLINE writeMatMulScalarCell #-}
writeMatMulScalarCell :: Int -> Arr Int -> Arr Int -> Arr Int -> Int -> Int -> Prog ()
writeMatMulScalarCell n left right out i j = do
  total <- accumFor n 0 $ \acc k -> do
    lhs <- readArr left (i * n + k)
    rhs <- readArr right (k * n + j)
    pure (acc + lhs * rhs)
  writeArr out (i * n + j) total

{-# INLINE writeMatMulVecChunkInt32 #-}
writeMatMulVecChunkInt32 :: Int -> Arr Int32 -> Arr Int32 -> Arr Int32 -> Int -> Int -> Prog ()
writeMatMulVecChunkInt32 n left right out i j0 = do
  total <- accumI32VecFor n (broadcastI32Vec 0) $ \acc k -> do
    lhs <- readArr left (i * n + k)
    rhs <- readI32Vec right (k * n + j0)
    pure (addI32Vec acc (mulI32Vec (broadcastI32Vec lhs) rhs))
  writeI32Vec out (i * n + j0) total

{-# INLINE writeMatMulScalarCellInt32 #-}
writeMatMulScalarCellInt32 :: Int -> Arr Int32 -> Arr Int32 -> Arr Int32 -> Int -> Int -> Prog ()
writeMatMulScalarCellInt32 n left right out i j = do
  total <- accumFor n 0 $ \acc k -> do
    lhs <- readArr left (i * n + k)
    rhs <- readArr right (k * n + j)
    pure (acc + lhs * rhs)
  writeArr out (i * n + j) total

{-# INLINE writeMatMulVecChunkDouble #-}
writeMatMulVecChunkDouble :: Int -> Arr Double -> Arr Double -> Arr Double -> Int -> Int -> Prog ()
writeMatMulVecChunkDouble n left right out i j0 = do
  total <- accumDVecFor n (broadcastDVec 0.0) $ \acc k -> do
    lhs <- readArr left (i * n + k)
    rhs <- readDVec right (k * n + j0)
    pure (addDVec acc (mulDVec (broadcastDVec lhs) rhs))
  writeDVec out (i * n + j0) total

{-# INLINE writeMatMulScalarCellDouble #-}
writeMatMulScalarCellDouble :: Int -> Arr Double -> Arr Double -> Arr Double -> Int -> Int -> Prog ()
writeMatMulScalarCellDouble n left right out i j = do
  total <- accumFor n 0.0 $ \acc k -> do
    lhs <- readArr left (i * n + k)
    rhs <- readArr right (k * n + j)
    pure (acc + lhs * rhs)
  writeArr out (i * n + j) total

runWavefrontEditDistanceKernel :: Int -> Int -> Arr Int -> Arr Int -> Arr Int -> IO ()
runWavefrontEditDistanceKernel rows cols left right dp =
  runProg $
    parallel $ do
      let tableCols = cols + 1
      for1 (rows + 1) Schedule.identity $ \i ->
        writeArr dp (i * tableCols) i
      barrier
      for1 cols Schedule.identity $ \j0 ->
        let j = j0 + 1
         in writeArr dp j j
      barrier
      for2 rows cols Schedule.wavefront $ \i0 j0 -> do
        let i = i0 + 1
            j = j0 + 1
            rowBase = i * tableCols
            prevRowBase = (i - 1) * tableCols
        deletion <- readArr dp (prevRowBase + j)
        insertion <- readArr dp (rowBase + (j - 1))
        substitution <- readArr dp (prevRowBase + (j - 1))
        x <- readArr left i0
        y <- readArr right j0
        let cost
              | x == y = 0
              | otherwise = 1
            !cell =
              min3
                (deletion + 1)
                (insertion + 1)
                (substitution + cost)
        writeArr dp (rowBase + j) cell

runRedBlackStencilKernel :: Int -> Arr Int -> Arr Int -> IO ()
runRedBlackStencilKernel n src out =
  runProg $
    parallel $ do
      for1 (n * n) Schedule.identity $ \idx -> do
        x <- readArr src idx
        writeArr out idx x
      barrier
      parForCheckerboard 0 (rect2 1 1 (n - 1) (n - 1)) $ \i j -> do
        let !base = i * n
        center <- readArr out (base + j)
        up     <- readArr out (base - n + j)
        down   <- readArr out (base + n + j)
        left   <- readArr out (base + j - 1)
        right  <- readArr out (base + j + 1)
        writeArr out (base + j) ((center + up + down + left + right) `quot` 5)
      barrier
      parForCheckerboard 1 (rect2 1 1 (n - 1) (n - 1)) $ \i j -> do
        let !base = i * n
        center <- readArr out (base + j)
        up     <- readArr out (base - n + j)
        down   <- readArr out (base + n + j)
        left   <- readArr out (base + j - 1)
        right  <- readArr out (base + j + 1)
        writeArr out (base + j) ((center + up + down + left + right) `quot` 5)

runThreePhaseNormalizeKernel :: Int -> Arr Double -> Arr Double -> IO ()
runThreePhaseNormalizeKernel n src out =
  runProg $
    parallel $ do
      total <- parFoldFor doubleSum n $ \i -> readArr src i
      barrier
      let mean = total / fromIntegral n
      sqTotal <- parFoldFor doubleSum n $ \i -> do
        x <- readArr src i
        let delta = x - mean
        pure (delta * delta)
      barrier
      let variance = sqTotal / fromIntegral n
          invStd
            | variance <= 0 = 0
            | otherwise = 1 / sqrt variance
      for1 n Schedule.identity $ \i -> do
        x <- readArr src i
        writeArr out i ((x - mean) * invStd)

runSeparableBlurKernel :: Int -> Arr Int -> Arr Int -> IO ()
runSeparableBlurKernel n src out = do
  tmp <- newArr (n * n)
  runSeparableBlurKernelWithScratch n src tmp out

runSeparableBlurKernelWithScratch :: Int -> Arr Int -> Arr Int -> Arr Int -> IO ()
runSeparableBlurKernelWithScratch n src tmp out =
  runProg $
    parallel $ do
      for2 n n Schedule.outerParallel $ \i j -> do
        let !base = i * n
            jL = clampIndex n (j - 1)
            jR = clampIndex n (j + 1)
        left   <- readArr src (base + jL)
        center <- readArr src (base + j)
        right  <- readArr src (base + jR)
        writeArr tmp (base + j) ((left + center + right) `quot` 3)
      barrier
      for2 n n Schedule.outerParallel $ \i j -> do
        let iU     = clampIndex n (i - 1)
            iD     = clampIndex n (i + 1)
            !base  = i * n
            upBase = iU * n
            dnBase = iD * n
        up     <- readArr tmp (upBase + j)
        center <- readArr tmp (base   + j)
        down   <- readArr tmp (dnBase + j)
        writeArr out (base + j) ((up + center + down) `quot` 3)

isInterior :: Int -> Int -> Int -> Bool
isInterior n i j = i > 0 && i + 1 < n && j > 0 && j + 1 < n

clampIndex :: Int -> Int -> Int
clampIndex n i
  | i < 0 = 0
  | i >= n = n - 1
  | otherwise = i

requirePolyhedral :: String -> Either Poly.PolyhedralError (Prog ()) -> Prog ()
requirePolyhedral label compiled =
  case compiled of
    Left err -> error (label ++ ": " ++ show err)
    Right prog -> prog

polyhedralTiledMatMulKernel :: Int -> Arr Int -> Arr Int -> Arr Int -> Either Poly.PolyhedralError (Prog ())
polyhedralTiledMatMulKernel n left right out =
  Poly.lowerKernel2D $
    Poly.kernel2D
      (sh2 n n)
      [ VerifyPoly.phase2DForVerifiedSchedule
          "tiled-matmul"
          (VerifyPoly.TileSchedule2D tile tile)
          Poly.IndependentDependence2D
          [ VerifyPoly.rowMajorRead2D "left" n Poly.rowVar (Poly.auxVar "k")
          , VerifyPoly.rowMajorRead2D "right" n (Poly.auxVar "k") Poly.colVar
          ]
          [VerifyPoly.rowMajorWrite2D "out" n Poly.rowVar Poly.colVar]
          (\i j -> writeMatMulScalarCell n left right out i j)
      ]
  where
    tile = max 1 (min 32 n)

polyhedralWavefrontKernel ::
  Int ->
  Int ->
  Int ->
  Arr Int ->
  Arr Int ->
  Arr Int ->
  Either Poly.PolyhedralError (Prog ())
polyhedralWavefrontKernel rows cols tableCols left right dp =
  Poly.lowerKernel2D $
    Poly.kernel2D
      (sh2 rows cols)
      [ VerifyPoly.phase2DForVerifiedSchedule
          "edit-distance-wavefront"
          VerifyPoly.WavefrontSchedule2D
          Poly.WavefrontDependence2D
          [ VerifyPoly.waveRowMajorRead2D "dp" tableCols (1, 1) Verify.WavePrevRow
          , VerifyPoly.waveRowMajorRead2D "dp" tableCols (1, 1) Verify.WavePrevCol
          , VerifyPoly.waveRowMajorRead2D "dp" tableCols (1, 1) Verify.WavePrevDiag
          , Poly.readAccess2D "left" Poly.rowVar
          , Poly.readAccess2D "right" Poly.colVar
          ]
          [VerifyPoly.waveRowMajorWrite2D "dp" tableCols (1, 1)]
          (\i0 j0 -> do
             let i = i0 + 1
                 j = j0 + 1
                 rowBase = i * tableCols
                 prevRowBase = (i - 1) * tableCols
             deletion <- readArr dp (prevRowBase + j)
             insertion <- readArr dp (rowBase + (j - 1))
             substitution <- readArr dp (prevRowBase + (j - 1))
             x <- readArr left i0
             y <- readArr right j0
             let cost
                   | x == y = 0
                   | otherwise = 1
                 !cell =
                   min3
                     (deletion + 1)
                     (insertion + 1)
                     (substitution + cost)
             writeArr dp (rowBase + j) cell)
      ]

min3 :: Int -> Int -> Int -> Int
min3 x y z = min x (min y z)

jacobi2DBenchmarkSteps :: Int
jacobi2DBenchmarkSteps = 50

runWavefrontLCSKernel :: Int -> Int -> Arr Int -> Arr Int -> Arr Int -> IO ()
runWavefrontLCSKernel rows cols left right dp =
  runProg $
    parallel $ do
      let tableCols = cols + 1
      for1 (rows + 1) Schedule.identity $ \i ->
        writeArr dp (i * tableCols) 0
      barrier
      for1 cols Schedule.identity $ \j0 ->
        let j = j0 + 1
         in writeArr dp j 0
      barrier
      for2 rows cols Schedule.wavefront $ \i0 j0 -> do
        let i = i0 + 1
            j = j0 + 1
            rowBase = i * tableCols
            prevRowBase = (i - 1) * tableCols
        x <- readArr left i0
        y <- readArr right j0
        if x == y
          then do
            diag <- readArr dp (prevRowBase + (j - 1))
            writeArr dp (rowBase + j) (diag + 1)
          else do
            fromUp   <- readArr dp (prevRowBase + j)
            fromLeft <- readArr dp (rowBase + (j - 1))
            writeArr dp (rowBase + j) (max fromUp fromLeft)

runJacobi2DStepKernel :: Int -> Arr Double -> Arr Double -> IO ()
runJacobi2DStepKernel n prev next =
  runProg $
    parallel $
      for2 (n - 2) (n - 2) Schedule.outerParallel $ \i0 j0 -> do
        let i = i0 + 1
            j = j0 + 1
            idx = i * n + j
        center <- readArr prev idx
        up    <- readArr prev (idx - n)
        down  <- readArr prev (idx + n)
        lv    <- readArr prev (idx - 1)
        rv    <- readArr prev (idx + 1)
        writeArr next idx ((center + up + down + lv + rv) * 0.2)

runTiled3DMapKernel :: Int -> Arr Int -> Arr Int -> Arr Int -> IO ()
runTiled3DMapKernel n left right out =
  runProg $
    parallel $
      for3 n n n Schedule.outerParallel $ \i j k -> do
        let idx = i * n * n + j * n + k
        x <- readArr left idx
        y <- readArr right idx
        writeArr out idx (x + 2 * y)

runWavefrontLCSExample :: [Int] -> [Int] -> IO Int
runWavefrontLCSExample xs ys = do
  left <- fromList xs
  right <- fromList ys
  let rows = length xs
      cols = length ys
      tableCols = cols + 1
  dp <- newArr ((rows + 1) * tableCols)
  runWavefrontLCSKernel rows cols left right dp
  readArrIO dp (rows * tableCols + cols)

runJacobi2DExample :: Int -> Int -> IO Int
runJacobi2DExample n steps = do
  input <- seededDoubleVector (n * n) 17 11
  current <- seededDoubleVector (n * n) 17 11
  scratch <- newArr (n * n)
  let go !remaining !cur !nxt
        | remaining <= 0 = pure cur
        | otherwise = do
            runJacobi2DStepKernel n cur nxt
            go (remaining - 1) nxt cur
  finalBuf <- go steps current scratch
  sampleDoubleMatrix finalBuf n

runTiled3DMapExample :: Int -> IO Int
runTiled3DMapExample n = do
  left  <- seededVector (n * n * n) 17 11
  right <- seededVector (n * n * n) 31 7
  out   <- newArr (n * n * n)
  runTiled3DMapKernel n left right out
  sampleVolume out n

sampleVector :: Arr Int -> Int -> IO Int
sampleVector arr n = do
  first <- readArrIO arr 0
  middle <- readArrIO arr (n `quot` 2)
  lastValue <- readArrIO arr (n - 1)
  pure (first + middle + lastValue)

sampleMatrix :: Arr Int -> Int -> IO Int
sampleMatrix arr n = do
  topLeft <- readArrIO arr 0
  center <- readArrIO arr (mid * n + mid)
  bottomRight <- readArrIO arr (n * n - 1)
  pure (topLeft + center + bottomRight)
  where
    mid = n `quot` 2

sampleInt32Matrix :: Arr Int32 -> Int -> IO Int
sampleInt32Matrix arr n = do
  topLeft <- readArrIO arr 0
  center <- readArrIO arr (mid * n + mid)
  bottomRight <- readArrIO arr (n * n - 1)
  pure (fromIntegral topLeft + fromIntegral center + fromIntegral bottomRight)
  where
    mid = n `quot` 2

sampleVolume :: Arr Int -> Int -> IO Int
sampleVolume arr n = do
  first <- readArrIO arr 0
  center <- readArrIO arr (((mid * n) + mid) * n + mid)
  lastValue <- readArrIO arr (n * n * n - 1)
  pure (first + center + lastValue)
  where
    mid = n `quot` 2

sampleDoubleMatrix :: Arr Double -> Int -> IO Int
sampleDoubleMatrix arr n = do
  topLeft <- readArrIO arr 0
  center <- readArrIO arr (mid * n + mid)
  bottomRight <- readArrIO arr (n * n - 1)
  pure (round ((topLeft + center + bottomRight) * 1000))
  where
    mid = n `quot` 2

sampleDoubleVector :: Arr Double -> Int -> IO Int
sampleDoubleVector arr n = do
  first <- readArrIO arr 0
  middle <- readArrIO arr (n `quot` 2)
  lastValue <- readArrIO arr (n - 1)
  pure (round ((first + middle + lastValue) * 1000))

sampleDoubleTriples :: Arr Double -> Arr Double -> Arr Double -> Int -> IO Int
sampleDoubleTriples arrX arrY arrZ n = do
  x0 <- readArrIO arrX 0
  y0 <- readArrIO arrY 0
  z0 <- readArrIO arrZ 0
  xm <- readArrIO arrX mid
  ym <- readArrIO arrY mid
  zm <- readArrIO arrZ mid
  xN <- readArrIO arrX (n - 1)
  yN <- readArrIO arrY (n - 1)
  zN <- readArrIO arrZ (n - 1)
  pure (round ((x0 + y0 + z0 + xm + ym + zm + xN + yN + zN) * 1000))
  where
    mid = n `quot` 2

interleave3 :: [Double] -> [Double] -> [Double] -> [Double]
interleave3 [] [] [] = []
interleave3 (x : xs) (y : ys) (z : zs) =
  x : y : z : interleave3 xs ys zs
interleave3 _ _ _ =
  error "interleave3: mismatched input lengths"
