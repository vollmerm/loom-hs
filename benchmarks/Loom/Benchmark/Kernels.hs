{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}

module Loom.Benchmark.Kernels
  ( Benchmark (..)
  , benchmarks
  , lookupBenchmark
  , runFill3DExample
  , runNBodyExample
  , runInt32TiledMatMulScalarExample
  , runInt32TiledMatMulVectorizedExample
  , runDoubleMatMulExample
  , runDoubleTiledMatMulScalarExample
  , runDoubleTiledMatMulVectorizedExample
  , runMatMulExample
  , runPolyhedralTiledMatMulExample
  , runPolyhedralWavefrontEditDistanceExample
  , runWavefrontEditDistanceExample
  , runRedBlackStencilExample
  , runThreePhaseNormalizeExample
  , runSeparableBlurExample
  ) where

import Loom
import qualified Loom.Polyhedral as Poly
import qualified Loom.Verify as Verify
import qualified Loom.Verify.Polyhedral as VerifyPoly
import Data.Int (Int32)

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

data NBodyEnv = NBodyEnv
  { nbodyCount :: !Int
  , nbodyPosX :: !(Arr Double)
  , nbodyPosY :: !(Arr Double)
  , nbodyPosZ :: !(Arr Double)
  , nbodyMass :: !(Arr Double)
  }

data DoubleMatrixEnv = DoubleMatrixEnv
  { doubleMatrixDim :: !Int
  , doubleMatrixLeft :: !(Arr Double)
  , doubleMatrixRight :: !(Arr Double)
  }

data Int32MatrixEnv = Int32MatrixEnv
  { int32MatrixDim :: !Int
  , int32MatrixLeft :: !(Arr Int32)
  , int32MatrixRight :: !(Arr Int32)
  }

data EditDistanceEnv = EditDistanceEnv
  { editRows :: !Int
  , editCols :: !Int
  , editLeft :: !(Arr Int)
  , editRight :: !(Arr Int)
  }

data Force3 =
  Force3
    {-# UNPACK #-} !Double
    {-# UNPACK #-} !Double
    {-# UNPACK #-} !Double

benchmarks :: [Benchmark]
benchmarks =
  [ Benchmark "fill" "parallel 1D fill into a fresh array" 1000000 setupFill runFill
  , Benchmark "fill-3d" "parallel 3D fill into a fresh volume" 128 setupFill runFill3D
  , Benchmark "map" "parallel 1D elementwise map over two inputs" 1000000 setupBinaryVector runMap
  , Benchmark "sum" "parallel reduction over one vector" 1000000 setupUnaryVector runSum
  , Benchmark "dot" "parallel dot product over two vectors" 1000000 setupBinaryVector runDot
  , Benchmark "nbody" "parallel softened all-pairs n-body force accumulation with SoA layout and source tiling" 2048 setupNBody runNBody
  , Benchmark "matmul" "parallel square matrix multiply" 256 setupMatrix runMatMul
  , Benchmark "tiled-matmul" "parallel square matrix multiply with tiled traversal" 256 setupMatrix runTiledMatMul
  , Benchmark "int32-tiled-matmul-scalar" "parallel square Int32 matrix multiply with tiling and scalar inner loops" 256 setupInt32Matrix runInt32TiledMatMulScalar
  , Benchmark "int32-tiled-matmul-vec" "parallel square Int32 matrix multiply with tiling and SIMD vectorization" 256 setupInt32Matrix runInt32TiledMatMulVectorized
  , Benchmark "double-matmul-scalar" "parallel square double matrix multiply without tiling or vectorization" 256 setupDoubleMatrix runDoubleMatMulScalar
  , Benchmark "double-tiled-matmul-scalar" "parallel square double matrix multiply with tiling and scalar inner loops" 256 setupDoubleMatrix runDoubleTiledMatMulScalar
  , Benchmark "double-tiled-matmul-vec" "parallel square double matrix multiply with tiling and SIMD vectorization" 256 setupDoubleMatrix runDoubleTiledMatMulVectorized
  , Benchmark "wavefront-edit-distance" "wavefront dynamic-programming edit distance" 1024 setupEditDistance runWavefrontEditDistance
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

setupNBody :: Int -> IO NBodyEnv
setupNBody n = do
  posX <- seededSignedDoubleVector n 17 11
  posY <- seededSignedDoubleVector n 31 7
  posZ <- seededSignedDoubleVector n 43 3
  mass <- seededPositiveDoubleVector n 29 5
  pure (NBodyEnv n posX posY posZ mass)

setupDoubleMatrix :: Int -> IO DoubleMatrixEnv
setupDoubleMatrix n = do
  a <- seededDoubleVector (n * n) 17 11
  b <- seededDoubleVector (n * n) 31 7
  pure (DoubleMatrixEnv n a b)

setupInt32Matrix :: Int -> IO Int32MatrixEnv
setupInt32Matrix n = do
  a <- seededInt32Vector (n * n) 17 11
  b <- seededInt32Vector (n * n) 31 7
  pure (Int32MatrixEnv n a b)

setupEditDistance :: Int -> IO EditDistanceEnv
setupEditDistance n = do
  left <- seededVector n 17 11
  right <- seededVector n 31 7
  pure (EditDistanceEnv n n left right)

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

runFill :: Int -> IO Int
runFill n = do
  arr <- newArr n
  runProg $
    parallel $
      parFor n $ \i ->
        writeArr arr i (i * 3 + 1)
  sampleVector arr n

runFill3D :: Int -> IO Int
runFill3D n = do
  let shape = sh3 n n n
  arr <- newArr (n * n * n)
  runProg $
    parallel $
      parForSh3 shape $ \ix ->
        withIx3 ix $ \i j k ->
          writeArr arr (index3 shape ix) (i * 1000000 + j * 1000 + k)
  sampleVolume arr n

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
  let shape = sh3 n n n
  arr <- newArr (n * n * n)
  runProg $
    parallel $
      parForSh3 shape $ \ix ->
        withIx3 ix $ \i j k ->
          writeArr arr (index3 shape ix) (i * 100 + j * 10 + k)
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
      parFor (rows + 1) $ \i ->
        writeArr dp (i * tableCols) i
      barrier
      parFor cols $ \j0 ->
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
      parFor n $ \i ->
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
      parFor n $ \i ->
        parFor n $ \j ->
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
      parFor n $ \i -> do
        xi <- readArr posX i
        yi <- readArr posY i
        zi <- readArr posZ i
        let fullTiles = n `quot` sourceTile
            tailStart = fullTiles * sourceTile
            tailCount = n - tailStart
        fullForce <-
          accumFor fullTiles zeroForce $ \acc tileIdx -> do
            let j0 = tileIdx * sourceTile
            tileForce <-
              accumFor sourceTile zeroForce $ \partial off ->
                accumulateNBodyForce posX posY posZ mass i xi yi zi partial (j0 + off)
            pure (addForce3 acc tileForce)
        Force3 ax ay az <-
          accumFor tailCount fullForce $ \acc off ->
            accumulateNBodyForce posX posY posZ mass i xi yi zi acc (tailStart + off)
        writeArr accX i ax
        writeArr accY i ay
        writeArr accZ i az
  where
    sourceTile = max 1 (min 64 n)

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
  Int ->
  Double ->
  Double ->
  Double ->
  Force3 ->
  Int ->
  Prog Force3
accumulateNBodyForce posX posY posZ mass i xi yi zi (Force3 ax ay az) j
  | i == j = pure (Force3 ax ay az)
  | otherwise = do
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
      parFor (rows + 1) $ \i ->
        writeArr dp (i * tableCols) i
      barrier
      parFor cols $ \j0 ->
        let j = j0 + 1
         in writeArr dp j j
      barrier
      parForWavefront2D (sh2 rows cols) $ \ix ->
        withIx2 ix $ \i0 j0 -> do
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
