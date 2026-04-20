# Loom

`loom-hs` is a Haskell library for writing high-performance parallel loop kernels with a small, shape-first API. To use it, you:
1. describe the iteration space,
2. optionally attach a schedule,
3. read and write arrays inside a parallel kernel,
4. run the program!

## Main API

```haskell
import Loom
import qualified Loom.Schedule as Schedule
```

The core entry points are:

- `shape`, `withSchedule`, and `forEach` for rank-polymorphic loops,
- `for1`, `for2`, and `for3` for the common fixed-rank cases,
- `newArr`, `readArr`, and `writeArr` for mutable arrays,
- `foldFor`, `newReducer`, and `reduce` for reductions,
- `runProg` and `parallel` to execute kernels.

### Example: tiled 2D map

```haskell
import Loom
import qualified Loom.Schedule as Schedule

increment2D :: Int -> Int -> Arr Double -> Arr Double -> IO ()
increment2D rows cols input output =
  runProg $ parallel $
    for2 rows cols (Schedule.tile2 32 32) $ \i j -> do
      let ix = i * cols + j
      x <- readArr input ix
      writeArr output ix (x + 1)
```

### Example: rank-polymorphic traversal

```haskell
import Loom
import qualified Loom.Schedule as Schedule

fill3D :: Int -> Int -> Int -> Arr Int -> IO ()
fill3D depth rows cols out =
  runProg $ parallel $
    forEach (shape [depth, rows, cols] `withSchedule` Schedule.tile [8, 8, 4]) $ \ix -> do
      let [d, i, j] = coords ix
          offset = linearIndex [depth, rows, cols] ix
      writeArr out offset (d + i + j)
```

## Schedules

`Loom.Schedule` provides the public schedule vocabulary:

- `identity`
- `tile`, `tile2`, `tile3`
- `permute`
- `interchange`
- `affine`
- `compose` and `(>>>)`

Example:

```haskell
for2 rows cols (Schedule.tile2 32 32 >>> Schedule.interchange) $ \i j -> ...
```

## Advanced modules

The library is split into a small front door plus explicit advanced modules.

| Module | Purpose |
| --- | --- |
| `Loom` | Default kernel DSL for most code |
| `Loom.Schedule` | Public schedule construction and composition |
| `Loom.Expert` | Low-level shapes, schedules, transforms, and loop forms |
| `Loom.Polyhedral` | 2D affine access descriptions, legality analysis, and lowering |
| `Loom.Verify` | Verified loop builders with explicit access metadata |
| `Loom.Verify.Polyhedral` | Bridge helpers between verified descriptions and polyhedral kernels |

## Theory

The repository also includes a `theory/` workspace for the Agda mechanization work behind
the schedule-typed kernel design. Typecheck the initial Agda project with:

```bash
cd theory
make check
```

## Building

Build the library, test suite, and benchmark executable with:

```bash
cabal build
```

Run the test suite with:

```bash
cabal test
```

## Benchmarks

The repository includes a benchmark executable with kernels such as fill, map, sum, dot, Jacobi, n-body, matrix multiplication, wavefront edit distance, and stencil-style examples.

List benchmarks with:

```bash
cabal run loom-benchmarks -- --list
```

Run one benchmark with explicit settings:

```bash
cabal run loom-benchmarks -- double-tiled-matmul-vec --size 256 --warmup 1 --iterations 5
```

## Core inspection

This library is performance-sensitive. When changing the loop DSL, schedules, transforms, reducers, or benchmark kernels, it is useful to inspect generated Core for representative kernels.

The repository includes a helper for simplified Core dumps:

```bash
./benchmarks/dump-core.sh
```

## Status

`loom-hs` is currently pre-1.0 and still evolving, but the public API is organized around:

- one obvious loop API in `Loom`,
- one obvious schedule API in `Loom.Schedule`,
- explicit advanced modules for expert, verified, and polyhedral workflows.

The benchmarks and tests in this repository also serve as concrete examples of the intended programming style.
