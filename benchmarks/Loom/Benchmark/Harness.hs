module Loom.Benchmark.Harness
  ( runHarness
  ) where

import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM, when)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Numeric (showFFloat)
import System.Exit (die)
import Text.Read (readMaybe)

import Loom.Benchmark.Kernels
  ( Benchmark (..)
  , benchmarks
  , lookupBenchmark
  )

data Config = Config
  { cfgBenchmarkName :: Maybe String
  , cfgSize :: Maybe Int
  , cfgWarmup :: Int
  , cfgIterations :: Int
  , cfgListOnly :: Bool
  }

defaultConfig :: Config
defaultConfig =
  Config
    { cfgBenchmarkName = Nothing
    , cfgSize = Nothing
    , cfgWarmup = 1
    , cfgIterations = 5
    , cfgListOnly = False
    }

runHarness :: [String] -> IO ()
runHarness args =
  case parseArgs defaultConfig args of
    Left "help requested" ->
      putStr usageText
    Left err ->
      die (err <> "\n\n" <> usageText)
    Right cfg
      | cfgListOnly cfg -> putStrLn benchmarkTable
      | otherwise ->
          case resolveBenchmark cfg of
            Left err ->
              die (err <> "\n\n" <> usageText)
            Right (benchmark, size) -> runSelected cfg benchmark size

parseArgs :: Config -> [String] -> Either String Config
parseArgs cfg [] = Right cfg
parseArgs _ ("--help" : _) = Left "help requested"
parseArgs cfg ("--list" : rest) =
  parseArgs cfg {cfgListOnly = True} rest
parseArgs cfg ("--benchmark" : name : rest) =
  parseArgs cfg {cfgBenchmarkName = Just name} rest
parseArgs _ ("--benchmark" : []) =
  Left "missing benchmark name after --benchmark"
parseArgs cfg ("--size" : value : rest) = do
  size <- parsePositive "size" value
  parseArgs cfg {cfgSize = Just size} rest
parseArgs _ ("--size" : []) =
  Left "missing size after --size"
parseArgs cfg ("--warmup" : value : rest) = do
  warmup <- parseNonNegative "warmup" value
  parseArgs cfg {cfgWarmup = warmup} rest
parseArgs _ ("--warmup" : []) =
  Left "missing iteration count after --warmup"
parseArgs cfg ("--iterations" : value : rest) = do
  iterations <- parsePositive "iterations" value
  parseArgs cfg {cfgIterations = iterations} rest
parseArgs _ ("--iterations" : []) =
  Left "missing iteration count after --iterations"
parseArgs cfg (arg : rest)
  | "--" `isPrefixOf` arg = Left ("unrecognized option: " <> arg)
  | otherwise =
      case cfgBenchmarkName cfg of
        Nothing -> parseArgs cfg {cfgBenchmarkName = Just arg} rest
        Just _ -> Left ("unexpected extra positional argument: " <> arg)

resolveBenchmark :: Config -> Either String (Benchmark, Int)
resolveBenchmark cfg = do
  name <-
    maybe
      (Left "missing benchmark name")
      Right
      (cfgBenchmarkName cfg)
  benchmark <-
    maybe
      (Left ("unknown benchmark: " <> name))
      Right
      (lookupBenchmark name)
  let size = fromMaybe (benchmarkDefaultSize benchmark) (cfgSize cfg)
  Right (benchmark, size)

runSelected :: Config -> Benchmark -> Int -> IO ()
runSelected cfg (Benchmark name description _ setup prepare run validate) size = do
  env <- setup size
  warmupChecksums <- replicateM (cfgWarmup cfg) $ do
    prepare env
    result <- run env
    validate env result
  let warmupChecksum =
        case reverse warmupChecksums of
          checksum : _ -> Just checksum
          [] -> Nothing
  results <- replicateM (cfgIterations cfg) $ do
    prepare env
    (timing, result) <- timeAction (run env)
    checksum <- validate env result
    pure (timing, checksum)
  let timings = map fst results
      checksums = map snd results
  assertStableChecksums checksums
  checksum <-
    case checksums of
      value : _ -> pure value
      [] -> die "benchmark produced no timed iterations"
  putStrLn ("benchmark=" <> name)
  putStrLn ("description=" <> description)
  putStrLn ("size=" <> show size)
  putStrLn ("warmup=" <> show (cfgWarmup cfg))
  putStrLn ("iterations=" <> show (cfgIterations cfg))
  forM_ warmupChecksum $ \value ->
    putStrLn ("warmup-checksum=" <> show value)
  putStrLn ("checksum=" <> show checksum)
  putStrLn ("total-ms=" <> renderMillis (sum timings))
  putStrLn ("avg-ms=" <> renderMillis (average timings))
  putStrLn ("min-ms=" <> renderMillis (minimum timings))
  putStrLn ("max-ms=" <> renderMillis (maximum timings))

timeAction :: IO a -> IO (Word64, a)
timeAction action = do
  start <- getMonotonicTimeNSec
  checksum <- action >>= evaluate
  end <- getMonotonicTimeNSec
  pure (end - start, checksum)

assertStableChecksums :: [Int] -> IO ()
assertStableChecksums [] =
  die "benchmark produced no timed iterations"
assertStableChecksums (expected : actuals) =
  forM_ actuals $ \actual ->
    when (actual /= expected) $
      die
        ( "benchmark checksum changed across iterations: expected "
            <> show expected
            <> ", got "
            <> show actual
        )

benchmarkTable :: String
benchmarkTable =
  unlines $
    ("available benchmarks: " <> intercalate ", " (map benchmarkName benchmarks))
      : [ "  "
            <> benchmarkName benchmark
            <> " (default size "
            <> show (benchmarkDefaultSize benchmark)
            <> "): "
            <> benchmarkDescription benchmark
        | benchmark <- benchmarks
        ]

usageText :: String
usageText =
  unlines
    [ "Usage:"
    , "  cabal run loom-benchmarks -- BENCHMARK [--size N] [--warmup N] [--iterations N] [+RTS -N -RTS]"
    , "  cabal run loom-benchmarks -- --list"
    , ""
    , benchmarkTable
    ]

parsePositive :: String -> String -> Either String Int
parsePositive label value = do
  parsed <- parseInt label value
  if parsed <= 0
    then Left (label <> " must be positive, got " <> show parsed)
    else Right parsed

parseNonNegative :: String -> String -> Either String Int
parseNonNegative label value = do
  parsed <- parseInt label value
  if parsed < 0
    then Left (label <> " must be non-negative, got " <> show parsed)
    else Right parsed

parseInt :: String -> String -> Either String Int
parseInt label value =
  maybe
    (Left ("could not parse " <> label <> " value: " <> value))
    Right
    (readMaybe value)

average :: [Word64] -> Word64
average [] = 0
average xs = sum xs `div` fromIntegral (length xs)

renderMillis :: Word64 -> String
renderMillis ns =
  showFFloat (Just 3) millis " ms"
  where
    millis :: Double
    millis = fromIntegral ns / 1000000.0

isPrefixOf :: String -> String -> Bool
isPrefixOf prefix value = take (length prefix) value == prefix
