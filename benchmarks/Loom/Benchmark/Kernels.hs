{-# LANGUAGE ExistentialQuantification #-}

module Loom.Benchmark.Kernels
  ( Benchmark (..)
  , benchmarks
  , lookupBenchmark
  , runRedBlackStencilExample
  , runThreePhaseNormalizeExample
  , runSeparableBlurExample
  ) where

import Loom

data Benchmark = forall env. Benchmark
  { benchmarkName :: String
  , benchmarkDescription :: String
  , benchmarkDefaultSize :: Int
  , benchmarkSetup :: Int -> IO env
  , benchmarkRun :: env -> IO Int
  }

data UnaryEnv = UnaryEnv
  { unaryLength :: !Int
  , unaryInput :: !(Arr Int)
  }

data BinaryEnv = BinaryEnv
  { binaryLength :: !Int
  , binaryLeft :: !(Arr Int)
  , binaryRight :: !(Arr Int)
  }

data MatrixEnv = MatrixEnv
  { matrixDim :: !Int
  , matrixLeft :: !(Arr Int)
  , matrixRight :: !(Arr Int)
  }

data ImageEnv = ImageEnv
  { imageDim :: !Int
  , imageInput :: !(Arr Int)
  }

data DoubleUnaryEnv = DoubleUnaryEnv
  { doubleLength :: !Int
  , doubleInput :: !(Arr Double)
  }

benchmarks :: [Benchmark]
benchmarks =
  [ Benchmark "fill" "parallel 1D fill into a fresh array" 1000000 setupFill runFill
  , Benchmark "map" "parallel 1D elementwise map over two inputs" 1000000 setupBinaryVector runMap
  , Benchmark "sum" "parallel reduction over one vector" 1000000 setupUnaryVector runSum
  , Benchmark "dot" "parallel dot product over two vectors" 1000000 setupBinaryVector runDot
  , Benchmark "matmul" "parallel square matrix multiply" 256 setupMatrix runMatMul
  , Benchmark "tiled-matmul" "parallel square matrix multiply with tiled traversal" 256 setupMatrix runTiledMatMul
  , Benchmark "red-black-stencil" "barriered in-place red/black 5-point stencil sweep" 1024 setupImage runRedBlackStencil
  , Benchmark "normalize-3phase" "three-phase normalization with two reductions and barriers" 1000000 setupDoubleVector runThreePhaseNormalize
  , Benchmark "separable-blur" "two-pass 2D blur with a barriered scratch handoff" 1024 setupImage runSeparableBlur
  ]

lookupBenchmark :: String -> Maybe Benchmark
lookupBenchmark target = go benchmarks
  where
    go [] = Nothing
    go (benchmark : rest)
      | benchmarkName benchmark == target = Just benchmark
      | otherwise = go rest

setupFill :: Int -> IO Int
setupFill = pure

setupUnaryVector :: Int -> IO UnaryEnv
setupUnaryVector n = do
  xs <- seededVector n 17 11
  pure (UnaryEnv n xs)

setupBinaryVector :: Int -> IO BinaryEnv
setupBinaryVector n = do
  xs <- seededVector n 17 11
  ys <- seededVector n 31 7
  pure (BinaryEnv n xs ys)

setupMatrix :: Int -> IO MatrixEnv
setupMatrix n = do
  a <- seededVector (n * n) 17 11
  b <- seededVector (n * n) 31 7
  pure (MatrixEnv n a b)

setupImage :: Int -> IO ImageEnv
setupImage n = do
  img <- seededVector (n * n) 17 11
  pure (ImageEnv n img)

setupDoubleVector :: Int -> IO DoubleUnaryEnv
setupDoubleVector n = do
  xs <- seededDoubleVector n 17 11
  pure (DoubleUnaryEnv n xs)

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

runFill :: Int -> IO Int
runFill n = do
  arr <- newArr n
  runProg $
    parallel $
      parFor n $ \i ->
        writeArr arr i (i * 3 + 1)
  sampleVector arr n

runMap :: BinaryEnv -> IO Int
runMap env = do
  let n = binaryLength env
  out <- newArr n
  runProg $
    parallel $
      parFor n $ \i -> do
        x <- readArr (binaryLeft env) i
        y <- readArr (binaryRight env) i
        writeArr out i (x + (2 * y))
  sampleVector out n

runSum :: UnaryEnv -> IO Int
runSum env =
  runProg $
    parallel $
      newReducer intSum $ \sumVar -> do
        parFor (unaryLength env) $ \i -> do
          x <- readArr (unaryInput env) i
          reduce sumVar x
        getReducer sumVar

runDot :: BinaryEnv -> IO Int
runDot env =
  runProg $
    parallel $
      newReducer intSum $ \sumVar -> do
        parFor (binaryLength env) $ \i -> do
          x <- readArr (binaryLeft env) i
          y <- readArr (binaryRight env) i
          reduce sumVar (x * y)
        getReducer sumVar

runMatMul :: MatrixEnv -> IO Int
runMatMul env = do
  let n = matrixDim env
      shape = sh2 n n
  out <- newArr (n * n)
  runProg $
    parallel $
      parForSh2 shape $ \outIx ->
        withIx2 outIx $ \i j -> do
          total <- accumFor n 0 $ \acc k -> do
            lhs <- readArr (matrixLeft env) (i * n + k)
            rhs <- readArr (matrixRight env) (k * n + j)
            pure (acc + lhs * rhs)
          writeArr out (index2 shape outIx) total
  sampleMatrix out n

runTiledMatMul :: MatrixEnv -> IO Int
runTiledMatMul env = do
  let n = matrixDim env
      shape = sh2 n n
      tile = max 1 (min 32 n)
  out <- newArr (n * n)
  runProg $
    parallel $
      tile2D tile tile shape $ \row0 col0 ->
        parForTile2D tile tile row0 col0 shape $ \i j -> do
          total <- accumFor n 0 $ \acc k -> do
            lhs <- readArr (matrixLeft env) (i * n + k)
            rhs <- readArr (matrixRight env) (k * n + j)
            pure (acc + lhs * rhs)
          writeArr out (i * n + j) total
  sampleMatrix out n

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

runSeparableBlurExample :: Int -> [Int] -> IO [Int]
runSeparableBlurExample n xs = do
  src <- fromList xs
  out <- newArr (n * n)
  runSeparableBlurKernel n src out
  toList out

runRedBlackStencilKernel :: Int -> Arr Int -> Arr Int -> IO ()
runRedBlackStencilKernel n src out =
  runProg $
    parallel $ do
      parFor (n * n) $ \idx -> do
        x <- readArr src idx
        writeArr out idx x
      barrier
      parForSh2 (sh2 n n) $ \ix ->
        withIx2 ix $ \i j ->
          if isInterior n i j && even (i + j)
            then do
              center <- readArr out (i * n + j)
              up <- readArr out ((i - 1) * n + j)
              down <- readArr out ((i + 1) * n + j)
              left <- readArr out (i * n + (j - 1))
              right <- readArr out (i * n + (j + 1))
              let total = center + up + down + left + right
              writeArr out (i * n + j) (total `quot` 5)
            else pure ()
      barrier
      parForSh2 (sh2 n n) $ \ix ->
        withIx2 ix $ \i j ->
          if isInterior n i j && odd (i + j)
            then do
              center <- readArr out (i * n + j)
              up <- readArr out ((i - 1) * n + j)
              down <- readArr out ((i + 1) * n + j)
              left <- readArr out (i * n + (j - 1))
              right <- readArr out (i * n + (j + 1))
              let total = center + up + down + left + right
              writeArr out (i * n + j) (total `quot` 5)
            else pure ()

runThreePhaseNormalizeKernel :: Int -> Arr Double -> Arr Double -> IO ()
runThreePhaseNormalizeKernel n src out =
  runProg $
    parallel $
      newReducer doubleSum $ \sumVar -> do
        parFor n $ \i -> do
          x <- readArr src i
          reduce sumVar x
        total <- getReducer sumVar
        barrier
        let mean = total / fromIntegral n
        newReducer doubleSum $ \sqVar -> do
          parFor n $ \i -> do
            x <- readArr src i
            let delta = x - mean
            reduce sqVar (delta * delta)
          sqTotal <- getReducer sqVar
          barrier
          let variance = sqTotal / fromIntegral n
              invStd
                | variance <= 0 = 0
                | otherwise = 1 / sqrt variance
          parFor n $ \i -> do
            x <- readArr src i
            writeArr out i ((x - mean) * invStd)

runSeparableBlurKernel :: Int -> Arr Int -> Arr Int -> IO ()
runSeparableBlurKernel n src out = do
  tmp <- newArr (n * n)
  runProg $
    parallel $ do
      parForSh2 (sh2 n n) $ \ix ->
        withIx2 ix $ \i j -> do
          let jL = clampIndex n (j - 1)
              jR = clampIndex n (j + 1)
          left <- readArr src (i * n + jL)
          center <- readArr src (i * n + j)
          right <- readArr src (i * n + jR)
          writeArr tmp (i * n + j) ((left + center + right) `quot` 3)
      barrier
      parForSh2 (sh2 n n) $ \ix ->
        withIx2 ix $ \i j -> do
          let iU = clampIndex n (i - 1)
              iD = clampIndex n (i + 1)
          up <- readArr tmp (iU * n + j)
          center <- readArr tmp (i * n + j)
          down <- readArr tmp (iD * n + j)
          writeArr out (i * n + j) ((up + center + down) `quot` 3)

isInterior :: Int -> Int -> Int -> Bool
isInterior n i j = i > 0 && i + 1 < n && j > 0 && j + 1 < n

clampIndex :: Int -> Int -> Int
clampIndex n i
  | i < 0 = 0
  | i >= n = n - 1
  | otherwise = i

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

sampleDoubleVector :: Arr Double -> Int -> IO Int
sampleDoubleVector arr n = do
  first <- readArrIO arr 0
  middle <- readArrIO arr (n `quot` 2)
  lastValue <- readArrIO arr (n - 1)
  pure (round ((first + middle + lastValue) * 1000))
