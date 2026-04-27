#!/usr/bin/env bash
set -euo pipefail

script_dir=$(cd "$(dirname "$0")" && pwd)
repo_root=$(cd "$script_dir/.." && pwd)
cd "$repo_root"

source "$script_dir/benchmark-common.sh"

usage() {
  cat <<'EOF'
Usage:
  benchmarks/compare-csv.sh [BENCHMARK ...] [options]

Options:
  --all                         Export every current Loom/C benchmark
  --benchmark NAME              Add a benchmark to the selection (repeatable)
  --benchmarks A,B,C             Add a comma-separated benchmark list
  --list                        Show the available benchmark list
  --warmup N                    Warmup iterations (default: 1)
  --iterations N                Timed iterations (default: 5)
  --threads N                   Set both Loom RTS threads and C OpenMP threads
  --loom-threads N              Set Loom RTS thread count only
  --c-threads N                 Set C/OpenMP thread count only
  --help                        Show this help

Each benchmark is exported at 10 sizes derived from its default size, from
small inputs up to slightly above the default size.
EOF
}

die() {
  printf '%s\n' "$*" >&2
  exit 1
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

print_list() {
  printf 'available benchmark exports:\n'
  local benchmark size
  for benchmark in "${common_benchmarks[@]}"; do
    size=$(benchmark_default_size "$benchmark")
    printf '  %s (default size %s)\n' "$benchmark" "$size"
  done
}

benchmark_csv_sizes() {
  local default_size=$1
  local multiplier size previous=""
  local -a sizes=()
  for multiplier in 1 2 3 4 5 6 7 8 9 10; do
    size=$((default_size * multiplier / 8))
    if [ "$size" -lt 1 ]; then
      size=1
    fi
    if [ "$size" != "$previous" ]; then
      sizes+=("$size")
      previous=$size
    fi
  done
  printf '%s\n' "${sizes[@]}"
}

selected_benchmarks=()
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
    --warmup)
      [ "$#" -ge 2 ] || die "missing value after --warmup"
      warmup=$2
      parse_non_negative "warmup" "$warmup" || exit 1
      shift
      ;;
    --iterations)
      [ "$#" -ge 2 ] || die "missing value after --iterations"
      iterations=$2
      parse_positive "iterations" "$iterations" || exit 1
      shift
      ;;
    --threads)
      [ "$#" -ge 2 ] || die "missing value after --threads"
      loom_threads=$2
      c_threads=$2
      parse_positive "threads" "$loom_threads" || exit 1
      shift
      ;;
    --loom-threads)
      [ "$#" -ge 2 ] || die "missing value after --loom-threads"
      loom_threads=$2
      parse_positive "loom-threads" "$loom_threads" || exit 1
      shift
      ;;
    --c-threads)
      [ "$#" -ge 2 ] || die "missing value after --c-threads"
      c_threads=$2
      parse_positive "c-threads" "$c_threads" || exit 1
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

printf 'config: warmup=%s iterations=%s' "$warmup" "$iterations"
if [ -n "$loom_threads" ]; then
  printf ' loom-threads=%s' "$loom_threads"
fi
if [ -n "$c_threads" ]; then
  printf ' c-threads=%s' "$c_threads"
fi
printf '\n\n'

if ! cabal build loom-benchmarks >/dev/null; then
  die "failed to build loom-benchmarks"
fi
loom_bin_path=$(cabal list-bin loom-benchmarks)
c_bin_path=$("$repo_root/benchmarks/c/build.sh")

data_dir="$script_dir/data"
mkdir -p "$data_dir"
run_date=$(date +%F)

for benchmark in "${selected_benchmarks[@]}"; do
  default_size=$(benchmark_default_size "$benchmark") || die "unknown benchmark: $benchmark"
  output_path="$data_dir/$benchmark-$run_date.csv"
  (
    tmp_output=$(mktemp "$data_dir/.${benchmark}.${run_date}.XXXXXX")
    trap 'rm -f "$tmp_output"' EXIT

    {
      printf 'benchmark,size,checksum,loom_avg_ms,c_avg_ms,speedup\n'
      while IFS= read -r size; do
        [ -n "$size" ] || continue

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

        speedup=$(awk -v c="$c_avg_ms" -v l="$loom_avg_ms" 'BEGIN { if (l == 0) print ""; else printf "%.2f", c / l }')
        printf '%s,%s,%s,%s,%s,%s\n' "$benchmark" "$size" "$loom_checksum" "$loom_avg_ms" "$c_avg_ms" "$speedup"
      done < <(benchmark_csv_sizes "$default_size")
    } >"$tmp_output"

    mv "$tmp_output" "$output_path"
    trap - EXIT
  )
  printf '%s\n' "$output_path"
done
