module Main (main) where

import System.Environment (getArgs)
import Loom.Benchmark.Harness (runHarness)

main :: IO ()
main = getArgs >>= runHarness
