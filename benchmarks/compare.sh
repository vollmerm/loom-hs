#!/usr/bin/env bash
set -euo pipefail

script_dir=$(cd "$(dirname "$0")" && pwd)
repo_root=$(cd "$script_dir/.." && pwd)
cd "$repo_root"

common_benchmarks=(
  fill
  fill-3d
  map
  sum
  dot
  jacobi-1d
  verified-jacobi-1d
  nbody
  matmul
  tiled-matmul
  int32-tiled-matmul-scalar
  int32-tiled-matmul-vec
  double-matmul-scalar
  double-tiled-matmul-scalar
  double-tiled-matmul-vec
  double-tiled-matmul-vec-newapi
  wavefront-edit-distance
  red-black-stencil
  normalize-3phase
  separable-blur
  wavefront-lcs
  jacobi-2d
  map-3d
)

usage() {
  cat <<'EOF'
Usage:
  benchmarks/compare.sh [BENCHMARK ...] [options]

Options:
  --all                         Compare every current Loom/C benchmark
  --benchmark NAME              Add a benchmark to the selection (repeatable)
  --benchmarks A,B,C             Add a comma-separated benchmark list
  --list                        Show the current comparison benchmark list
  --size N                      Run a single input size
  --size-range START:END[:STEP]  Run a range of sizes (default step: 1)
  --warmup N                    Warmup iterations (default: 1)
  --iterations N                Timed iterations (default: 5)
  --threads N                   Set both Loom RTS threads and C OpenMP threads
  --loom-threads N              Set Loom RTS thread count only
  --c-threads N                 Set C/OpenMP thread count only
  --help                        Show this help

Examples:
  benchmarks/compare.sh matmul --size-range 64:256:64 --warmup 1 --iterations 5 --threads 4
  cd benchmarks && ./compare.sh --all --size 256
EOF
}

die() {
  printf '%s\n' "$*" >&2
  exit 1
}

contains_benchmark() {
  local needle=$1
  shift
  local item
  for item in "$@"; do
    if [ "$item" = "$needle" ]; then
      return 0
    fi
  done
  return 1
}

parse_positive() {
  local label=$1
  local value=$2
  case $value in
    (''|*[!0-9]*)
      die "invalid $label value: $value"
      ;;
  esac
  if [ "$value" -le 0 ]; then
    die "$label must be positive, got $value"
  fi
}

parse_non_negative() {
  local label=$1
  local value=$2
  case $value in
    (''|*[!0-9]*)
      die "invalid $label value: $value"
      ;;
  esac
  if [ "$value" -lt 0 ]; then
    die "$label must be non-negative, got $value"
  fi
}

benchmark_default_size() {
  case $1 in
    fill) printf '%s\n' 1000000 ;;
    fill-3d) printf '%s\n' 128 ;;
    map) printf '%s\n' 1000000 ;;
    sum) printf '%s\n' 1000000 ;;
    dot) printf '%s\n' 1000000 ;;
    jacobi-1d) printf '%s\n' 262144 ;;
    verified-jacobi-1d) printf '%s\n' 262144 ;;
    nbody) printf '%s\n' 2048 ;;
    matmul) printf '%s\n' 256 ;;
    tiled-matmul) printf '%s\n' 256 ;;
    int32-tiled-matmul-scalar) printf '%s\n' 256 ;;
    int32-tiled-matmul-vec) printf '%s\n' 256 ;;
    double-matmul-scalar) printf '%s\n' 256 ;;
    double-tiled-matmul-scalar) printf '%s\n' 256 ;;
    double-tiled-matmul-vec) printf '%s\n' 256 ;;
    double-tiled-matmul-vec-newapi) printf '%s\n' 256 ;;
    wavefront-edit-distance) printf '%s\n' 1024 ;;
    red-black-stencil) printf '%s\n' 1024 ;;
    normalize-3phase) printf '%s\n' 1000000 ;;
    separable-blur) printf '%s\n' 1024 ;;
    wavefront-lcs) printf '%s\n' 1024 ;;
    jacobi-2d) printf '%s\n' 512 ;;
    map-3d) printf '%s\n' 128 ;;
    *) return 1 ;;
  esac
}

print_list() {
  printf 'current comparison benchmarks:\n'
  local benchmark size
  for benchmark in "${common_benchmarks[@]}"; do
    size=$(benchmark_default_size "$benchmark")
    printf '  %s (default size %s)\n' "$benchmark" "$size"
  done
}

parse_range() {
  local spec=$1
  IFS=: read -r range_start range_end range_step extra <<<"$spec"
  [ -n "${range_start:-}" ] || die "invalid size-range: $spec"
  [ -n "${range_end:-}" ] || die "invalid size-range: $spec"
  [ -z "${extra:-}" ] || die "invalid size-range: $spec"
  range_step=${range_step:-1}
  parse_positive "size-range start" "$range_start"
  parse_positive "size-range end" "$range_end"
  parse_positive "size-range step" "$range_step"
  if [ "$range_start" -gt "$range_end" ]; then
    die "size-range start must not exceed end: $spec"
  fi
}

extract_field() {
  local key=$1
  local text=$2
  printf '%s\n' "$text" | awk -F= -v key="$key" '
    $1 == key {
      print substr($0, length($1) + 2)
      exit
    }
  '
}

strip_ms() {
  printf '%s' "${1% ms}"
}

run_and_capture() {
  local label=$1
  shift
  local output
  if ! output=$("$@" 2>&1); then
    printf '%s\n' "$output" >&2
    die "$label failed"
  fi
  printf '%s' "$output"
}

print_row() {
  local benchmark=$1
  local size=$2
  local checksum=$3
  local loom_avg=$4
  local c_avg=$5
  local speedup
  speedup=$(awk -v c="$c_avg" -v l="$loom_avg" 'BEGIN { if (l == 0) print "n/a"; else printf "%.2fx", c / l }')
  printf '%-28s %8s %14s %14s %14s %9s\n' "$benchmark" "$size" "$checksum" "$loom_avg" "$c_avg" "$speedup"
}

selected_benchmarks=()
size_single=""
range_spec=""
warmup=1
iterations=5
loom_threads=""
c_threads=""
list_only=0

while [ "$#" -gt 0 ]; do
  case $1 in
    --help)
      usage
      exit 0
      ;;
    --list)
      list_only=1
      ;;
    --all)
      selected_benchmarks=("${common_benchmarks[@]}")
      ;;
    --benchmark)
      [ "$#" -ge 2 ] || die "missing benchmark name after --benchmark"
      if ! contains_benchmark "$2" "${selected_benchmarks[@]}"; then
        selected_benchmarks+=("$2")
      fi
      shift
      ;;
    --benchmarks)
      [ "$#" -ge 2 ] || die "missing benchmark list after --benchmarks"
      IFS=, read -r -a benchmark_list <<<"$2"
      local_item=""
      for local_item in "${benchmark_list[@]}"; do
        [ -n "$local_item" ] || continue
        if ! contains_benchmark "$local_item" "${selected_benchmarks[@]}"; then
          selected_benchmarks+=("$local_item")
        fi
      done
      shift
      ;;
    --size)
      [ "$#" -ge 2 ] || die "missing size after --size"
      size_single=$2
      parse_positive "size" "$size_single"
      shift
      ;;
    --size-range)
      [ "$#" -ge 2 ] || die "missing range after --size-range"
      range_spec=$2
      parse_range "$range_spec"
      shift
      ;;
    --warmup)
      [ "$#" -ge 2 ] || die "missing value after --warmup"
      warmup=$2
      parse_non_negative "warmup" "$warmup"
      shift
      ;;
    --iterations)
      [ "$#" -ge 2 ] || die "missing value after --iterations"
      iterations=$2
      parse_positive "iterations" "$iterations"
      shift
      ;;
    --threads)
      [ "$#" -ge 2 ] || die "missing value after --threads"
      loom_threads=$2
      c_threads=$2
      parse_positive "threads" "$loom_threads"
      shift
      ;;
    --loom-threads)
      [ "$#" -ge 2 ] || die "missing value after --loom-threads"
      loom_threads=$2
      parse_positive "loom-threads" "$loom_threads"
      shift
      ;;
    --c-threads)
      [ "$#" -ge 2 ] || die "missing value after --c-threads"
      c_threads=$2
      parse_positive "c-threads" "$c_threads"
      shift
      ;;
    --*)
      die "unrecognized option: $1"
      ;;
    *)
      if ! contains_benchmark "$1" "${selected_benchmarks[@]}"; then
        selected_benchmarks+=("$1")
      fi
      ;;
  esac
  shift
done

if [ "$list_only" -eq 1 ]; then
  print_list
  exit 0
fi

if [ "${#selected_benchmarks[@]}" -eq 0 ]; then
  selected_benchmarks=("${common_benchmarks[@]}")
fi

for benchmark in "${selected_benchmarks[@]}"; do
  if ! contains_benchmark "$benchmark" "${common_benchmarks[@]}"; then
    die "unknown benchmark: $benchmark"
  fi
done

if [ -n "$size_single" ] && [ -n "$range_spec" ]; then
  die "use either --size or --size-range, not both"
fi

sizes=()
if [ -n "$size_single" ]; then
  sizes+=("$size_single")
elif [ -n "$range_spec" ]; then
  IFS=: read -r range_start range_end range_step _ <<<"$range_spec"
  size=$range_start
  while [ "$size" -le "$range_end" ]; do
    sizes+=("$size")
    size=$((size + range_step))
  done
fi

printf 'config: warmup=%s iterations=%s' "$warmup" "$iterations"
if [ -n "$loom_threads" ]; then
  printf ' loom-threads=%s' "$loom_threads"
fi
if [ -n "$c_threads" ]; then
  printf ' c-threads=%s' "$c_threads"
fi
printf '\n'
printf '\n'
printf '%-28s %8s %14s %14s %14s %9s\n' benchmark size checksum loom-avg-ms c-avg-ms speedup

loom_bin_path=""
if ! cabal build loom-benchmarks >/dev/null; then
  die "failed to build loom-benchmarks"
fi
loom_bin_path=$(cabal list-bin loom-benchmarks)

c_bin_path=$("$repo_root/benchmarks/c/build.sh")

for benchmark in "${selected_benchmarks[@]}"; do
  benchmark_sizes=()
  if [ -n "$size_single" ] || [ -n "$range_spec" ]; then
    benchmark_sizes=("${sizes[@]}")
  else
    benchmark_sizes=("$(benchmark_default_size "$benchmark")")
  fi

  for size in "${benchmark_sizes[@]}"; do
    loom_cmd=("$loom_bin_path" "$benchmark" --size "$size" --warmup "$warmup" --iterations "$iterations")
    if [ -n "$loom_threads" ]; then
      loom_cmd+=("+RTS" "-N$loom_threads" "-RTS")
    fi

    loom_output=$(run_and_capture "loom benchmark $benchmark size $size" "${loom_cmd[@]}")
    if [ -n "$c_threads" ]; then
      if ! c_output=$(OMP_NUM_THREADS="$c_threads" "$c_bin_path" "$benchmark" --size "$size" --warmup "$warmup" --iterations "$iterations" 2>&1); then
        printf '%s\n' "$c_output" >&2
        die "c/openmp baseline $benchmark size $size failed"
      fi
    else
      if ! c_output=$("$c_bin_path" "$benchmark" --size "$size" --warmup "$warmup" --iterations "$iterations" 2>&1); then
        printf '%s\n' "$c_output" >&2
        die "c/openmp baseline $benchmark size $size failed"
      fi
    fi

    loom_name=$(extract_field benchmark "$loom_output")
    loom_size=$(extract_field size "$loom_output")
    loom_checksum=$(extract_field checksum "$loom_output")
    loom_avg_ms=$(strip_ms "$(extract_field avg-ms "$loom_output")")

    c_name=$(extract_field benchmark "$c_output")
    c_size=$(extract_field size "$c_output")
    c_checksum=$(extract_field checksum "$c_output")
    c_avg_ms=$(strip_ms "$(extract_field avg-ms "$c_output")")

    [ "$loom_name" = "$benchmark" ] || die "loom benchmark name mismatch: expected $benchmark, got $loom_name"
    [ "$c_name" = "$benchmark" ] || die "c benchmark name mismatch: expected $benchmark, got $c_name"
    [ "$loom_size" = "$size" ] || die "loom size mismatch for $benchmark: expected $size, got $loom_size"
    [ "$c_size" = "$size" ] || die "c size mismatch for $benchmark: expected $size, got $c_size"
    [ "$loom_checksum" = "$c_checksum" ] || die "checksum mismatch for $benchmark size $size: loom=$loom_checksum c=$c_checksum"

    print_row "$benchmark" "$size" "$loom_checksum" "$loom_avg_ms" "$c_avg_ms"
  done
done
