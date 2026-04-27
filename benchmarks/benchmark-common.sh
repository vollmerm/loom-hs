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
  stencil-pipeline
  wavefront-lcs
  jacobi-2d
  map-3d
)

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
      printf '%s\n' "invalid $label value: $value" >&2
      return 1
      ;;
  esac
  if [ "$value" -le 0 ]; then
    printf '%s\n' "$label must be positive, got $value" >&2
    return 1
  fi
}

parse_non_negative() {
  local label=$1
  local value=$2
  case $value in
    (''|*[!0-9]*)
      printf '%s\n' "invalid $label value: $value" >&2
      return 1
      ;;
  esac
  if [ "$value" -lt 0 ]; then
    printf '%s\n' "$label must be non-negative, got $value" >&2
    return 1
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
    stencil-pipeline) printf '%s\n' 512 ;;
    wavefront-lcs) printf '%s\n' 1024 ;;
    jacobi-2d) printf '%s\n' 512 ;;
    map-3d) printf '%s\n' 128 ;;
    *) return 1 ;;
  esac
}
