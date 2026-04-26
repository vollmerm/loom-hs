#!/usr/bin/env bash
set -eu

script_dir=$(cd "$(dirname "$0")" && pwd)
out="$script_dir/loom-openmp-baselines"
src_helpers="$script_dir/loom_benchmark_helpers.c"
src_main="$script_dir/openmp_baselines.c"
header="$script_dir/loom_benchmark_helpers.h"

needs_build=0
if [ ! -x "$out" ]; then
  needs_build=1
elif [ "$out" -ot "$src_helpers" ] || [ "$out" -ot "$src_main" ] || [ "$out" -ot "$header" ]; then
  needs_build=1
fi

if [ "$needs_build" -eq 1 ]; then
  cc_bin=${CC:-cc}
  os_name=$(uname -s)
  if [ "$os_name" = Darwin ]; then
    libomp_prefix=${OMP_PREFIX:-$(brew --prefix libomp)}
    "$cc_bin" -O2 -std=c11 -Wall -Wextra -Wpedantic -Wno-c23-extensions \
      -Xpreprocessor -fopenmp \
      -I"$libomp_prefix/include" \
      "$src_helpers" "$src_main" \
      -L"$libomp_prefix/lib" -lomp -lm \
      -o "$out"
  else
    "$cc_bin" -O2 -std=c11 -Wall -Wextra -Wpedantic -fopenmp \
      "$src_helpers" "$src_main" \
      -lm \
      -o "$out"
  fi
fi

printf '%s\n' "$out"
