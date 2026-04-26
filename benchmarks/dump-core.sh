#!/usr/bin/env bash

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${1:-$(mktemp -d "${TMPDIR:-/tmp}/loom-bench-core.XXXXXX")}"

mkdir -p "$out_dir"

cd "$repo_root"

cabal exec -- ghc \
  -O2 \
  -fllvm \
  -pgmlo=/opt/homebrew/opt/llvm@16/bin/opt \
  -pgmlc=/opt/homebrew/opt/llvm@16/bin/llc \
  -package primitive \
  -fforce-recomp \
  -i./src \
  -i./benchmarks \
  -outputdir "$out_dir" \
  -ddump-simpl \
  -ddump-to-file \
  -dsuppress-all \
  src/Loom/Internal/Kernel.hs \
  src/Loom/Internal/Verify.hs \
  src/Loom.hs \
  src/Loom/Verify.hs \
  src/Loom/Verify/Polyhedral.hs \
  benchmarks/Loom/Benchmark/Kernels.hs \
  benchmarks/Loom/Benchmark/Harness.hs \
  benchmarks/Main.hs \
  -o "$out_dir/loom-benchmarks-core" \
  >/dev/null

echo "Generated simplified Core under: $out_dir"
echo "Useful dump files include:"
echo "  $out_dir/Main.dump-simpl"
echo "  $out_dir/Loom.Benchmark.Kernels.dump-simpl"
echo "  $out_dir/Loom.Verify.dump-simpl"
