#include "loom_benchmark_helpers.h"

typedef int64_t checksum_t;


#include <inttypes.h>
#include <math.h>
#include <omp.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
  BENCH_FILL,
  BENCH_FILL_3D,
  BENCH_MAP,
  BENCH_SUM,
  BENCH_DOT,
  BENCH_JACOBI_1D,
  BENCH_VERIFIED_JACOBI_1D,
  BENCH_NBODY,
  BENCH_MATMUL,
  BENCH_TILED_MATMUL,
  BENCH_INT32_TILED_MATMUL_SCALAR,
  BENCH_INT32_TILED_MATMUL_VEC,
  BENCH_DOUBLE_MATMUL_SCALAR,
  BENCH_DOUBLE_TILED_MATMUL_SCALAR,
  BENCH_DOUBLE_TILED_MATMUL_VEC,
  BENCH_DOUBLE_TILED_MATMUL_VEC_NEWAPI,
  BENCH_WAVEFRONT_EDIT_DISTANCE,
  BENCH_RED_BLACK_STENCIL,
  BENCH_NORMALIZE_3PHASE,
  BENCH_SEPARABLE_BLUR,
  BENCH_WAVEFRONT_LCS,
  BENCH_JACOBI_2D,
  BENCH_TILED_3D_MAP
} benchmark_kind_t;

typedef struct {
  benchmark_kind_t kind;
  const char *name;
  const char *description;
  int default_size;
} benchmark_spec_t;

typedef struct {
  const char *benchmark_name;
  int size;
  int warmup;
  int iterations;
  bool list_only;
} config_t;

typedef struct {
  const char *benchmark;
  const char *description;
  int size;
  int warmup;
  int iterations;
  int has_warmup_checksum;
  checksum_t warmup_checksum;
  checksum_t checksum;
  double total_ms;
  double min_ms;
  double max_ms;
} report_t;

typedef void (*prepare_fn)(void *env);
typedef void (*run_fn)(void *env);
typedef checksum_t (*validate_fn)(void *env);

typedef struct {
  int n;
  int *out;
} FillEnv;

typedef struct {
  int n;
  int *out;
} Fill3DEnv;

typedef struct {
  int n;
  int *left;
  int *right;
  int *out;
} BinaryI64Env;

typedef struct {
  int n;
  int *input;
  checksum_t value;
} ReduceI64Env;

typedef struct {
  int n;
  int *left;
  int *right;
  checksum_t value;
} DotEnv;

typedef struct {
  int n;
  int steps;
  double *input;
  double *current;
  double *scratch;
  double *final_buffer;
} JacobiEnv;

typedef struct {
  int n;
  double *pos_x;
  double *pos_y;
  double *pos_z;
  double *mass;
  double *acc_x;
  double *acc_y;
  double *acc_z;
} NBodyEnv;

typedef struct {
  int n;
  int *left;
  int *right;
  int *out;
} MatrixI64Env;

typedef struct {
  int n;
  int32_t *left;
  int32_t *right;
  int32_t *out;
} MatrixI32Env;

typedef struct {
  int n;
  double *left;
  double *right;
  double *out;
} DoubleMatrixEnv;

typedef struct {
  int rows;
  int cols;
  int *left;
  int *right;
  int *dp;
} WavefrontEnv;

typedef struct {
  int n;
  int *input;
  int *output;
} ImageEnv;

typedef struct {
  int n;
  double *input;
  double *output;
} NormalizeEnv;

typedef struct {
  int n;
  int *input;
  int *scratch;
  int *output;
} BlurEnv;

typedef struct {
  int rows;
  int cols;
  int *left;
  int *right;
  int *dp;
} LCSEnv;

typedef struct {
  int n;
  int steps;
  double *input;
  double *current;
  double *scratch;
  double *final_buffer;
} Jacobi2DEnv;

typedef struct {
  int n;
  int *left;
  int *right;
  int *out;
} Volume3DMapEnv;

static const benchmark_spec_t benchmark_specs[] = {
    {BENCH_FILL, "fill", "parallel 1D fill into a fresh array", 1000000},
    {BENCH_FILL_3D, "fill-3d", "parallel 3D fill into a fresh volume", 128},
    {BENCH_MAP, "map", "parallel 1D elementwise map over two inputs", 1000000},
    {BENCH_SUM, "sum", "parallel reduction over one vector", 1000000},
    {BENCH_DOT, "dot", "parallel dot product over two vectors", 1000000},
    {BENCH_JACOBI_1D, "jacobi-1d", "vectorized 1D Jacobi / heat-diffusion timesteps with double buffering", 262144},
    {BENCH_VERIFIED_JACOBI_1D, "verified-jacobi-1d", "vectorized 1D Jacobi via Loom.Verify with checked shifted accesses", 262144},
    {BENCH_NBODY, "nbody", "parallel softened all-pairs n-body force accumulation with SoA layout and source tiling", 2048},
    {BENCH_MATMUL, "matmul", "parallel square matrix multiply", 256},
    {BENCH_TILED_MATMUL, "tiled-matmul", "parallel square matrix multiply with tiled traversal", 256},
    {BENCH_INT32_TILED_MATMUL_SCALAR, "int32-tiled-matmul-scalar", "parallel square Int32 matrix multiply with tiling and scalar inner loops", 256},
    {BENCH_INT32_TILED_MATMUL_VEC, "int32-tiled-matmul-vec", "parallel square Int32 matrix multiply with tiling and SIMD vectorization", 256},
    {BENCH_DOUBLE_MATMUL_SCALAR, "double-matmul-scalar", "parallel square double matrix multiply without tiling or vectorization", 256},
    {BENCH_DOUBLE_TILED_MATMUL_SCALAR, "double-tiled-matmul-scalar", "parallel square double matrix multiply with tiling and scalar inner loops", 256},
    {BENCH_DOUBLE_TILED_MATMUL_VEC, "double-tiled-matmul-vec", "parallel square double matrix multiply with tiling and SIMD vectorization", 256},
    {BENCH_DOUBLE_TILED_MATMUL_VEC_NEWAPI, "double-tiled-matmul-vec-newapi", "parallel square double matrix multiply with tiling and SIMD vectorization via shape-first Loom API", 256},
    {BENCH_WAVEFRONT_EDIT_DISTANCE, "wavefront-edit-distance", "wavefront dynamic-programming edit distance", 1024},
    {BENCH_RED_BLACK_STENCIL, "red-black-stencil", "barriered in-place red/black 5-point stencil sweep", 1024},
    {BENCH_NORMALIZE_3PHASE, "normalize-3phase", "three-phase normalization with two reductions and barriers", 1000000},
    {BENCH_SEPARABLE_BLUR, "separable-blur", "two-pass 2D blur with a barriered scratch handoff", 1024},
    {BENCH_WAVEFRONT_LCS, "wavefront-lcs", "wavefront longest-common-subsequence DP", 1024},
    {BENCH_JACOBI_2D, "jacobi-2d", "double-buffered 2D Jacobi heat diffusion, 50 steps", 512},
    {BENCH_TILED_3D_MAP, "map-3d", "flat 3D elementwise map using parFor3", 128},
};

static const size_t benchmark_count = sizeof(benchmark_specs) / sizeof(benchmark_specs[0]);

static inline size_t index2(int n, int i, int j) {
  return (size_t)i * (size_t)n + (size_t)j;
}

static inline size_t index3(int n, int i, int j, int k) {
  return (((size_t)i * (size_t)n) + (size_t)j) * (size_t)n + (size_t)k;
}

static inline int min_int(int a, int b) {
  return a < b ? a : b;
}

static inline int64_t min_i64(int64_t a, int64_t b) {
  return a < b ? a : b;
}

static inline int max_int(int a, int b) {
  return a > b ? a : b;
}

static const benchmark_spec_t *lookup_spec(const char *name) {
  for (size_t i = 0; i < benchmark_count; ++i) {
    if (strcmp(benchmark_specs[i].name, name) == 0) {
      return &benchmark_specs[i];
    }
  }
  return NULL;
}

static void print_benchmark_table(void) {
  fputs("available benchmarks: ", stdout);
  for (size_t i = 0; i < benchmark_count; ++i) {
    fputs(benchmark_specs[i].name, stdout);
    if (i + 1 < benchmark_count) {
      fputs(", ", stdout);
    }
  }
  fputc('\n', stdout);
  for (size_t i = 0; i < benchmark_count; ++i) {
    printf("  %s (default size %d): %s\n", benchmark_specs[i].name, benchmark_specs[i].default_size, benchmark_specs[i].description);
  }
}

static void usage(const char *prog) {
  printf("Usage:\n");
  printf("  %s BENCHMARK [--size N] [--warmup N] [--iterations N] [+RTS -N -RTS]\n", prog);
  printf("  %s --list\n", prog);
  printf("\n");
  print_benchmark_table();
}

static void die_usage(const char *prog, const char *message) {
  fprintf(stderr, "%s\n\n", message);
  usage(prog);
  exit(1);
}

static int parse_positive(const char *label, const char *value) {
  char *end = NULL;
  long parsed = strtol(value, &end, 10);
  if (end == value || *end != '\0' || parsed <= 0 || parsed > 1L << 30) {
    fprintf(stderr, "invalid %s value: %s\n", label, value);
    exit(1);
  }
  return (int)parsed;
}

static int parse_non_negative(const char *label, const char *value) {
  char *end = NULL;
  long parsed = strtol(value, &end, 10);
  if (end == value || *end != '\0' || parsed < 0 || parsed > 1L << 30) {
    fprintf(stderr, "invalid %s value: %s\n", label, value);
    exit(1);
  }
  return (int)parsed;
}

static config_t parse_args(const char *prog, int argc, char **argv) {
  config_t cfg;
  cfg.benchmark_name = NULL;
  cfg.size = -1;
  cfg.warmup = 1;
  cfg.iterations = 5;
  cfg.list_only = false;

  if (argc < 2) {
    usage(prog);
    exit(1);
  }

  for (int i = 1; i < argc; ++i) {
    if (strcmp(argv[i], "--help") == 0) {
      usage(prog);
      exit(0);
    } else if (strcmp(argv[i], "--list") == 0) {
      cfg.list_only = true;
    } else if (strcmp(argv[i], "--benchmark") == 0) {
      if (i + 1 >= argc) {
        die_usage(prog, "missing benchmark name after --benchmark");
      }
      cfg.benchmark_name = argv[++i];
    } else if (strcmp(argv[i], "--size") == 0) {
      if (i + 1 >= argc) {
        die_usage(prog, "missing size after --size");
      }
      cfg.size = parse_positive("size", argv[++i]);
    } else if (strcmp(argv[i], "--warmup") == 0) {
      if (i + 1 >= argc) {
        die_usage(prog, "missing iteration count after --warmup");
      }
      cfg.warmup = parse_non_negative("warmup", argv[++i]);
    } else if (strcmp(argv[i], "--iterations") == 0) {
      if (i + 1 >= argc) {
        die_usage(prog, "missing iteration count after --iterations");
      }
      cfg.iterations = parse_positive("iterations", argv[++i]);
    } else if (strncmp(argv[i], "--", 2) == 0) {
      die_usage(prog, "unrecognized option");
    } else if (cfg.benchmark_name == NULL) {
      cfg.benchmark_name = argv[i];
    } else {
      die_usage(prog, "unexpected extra positional argument");
    }
  }

  return cfg;
}

static void prepare_noop(void *env) {
  (void)env;
}

static void print_report(const report_t *report) {
  printf("benchmark=%s\n", report->benchmark);
  printf("description=%s\n", report->description);
  printf("size=%d\n", report->size);
  printf("warmup=%d\n", report->warmup);
  printf("iterations=%d\n", report->iterations);
  if (report->has_warmup_checksum) {
    printf("warmup-checksum=%" PRId64 "\n", report->warmup_checksum);
  }
  printf("checksum=%" PRId64 "\n", report->checksum);
  printf("total-ms=%.3f ms\n", report->total_ms);
  printf("avg-ms=%.3f ms\n", report->total_ms / (double)report->iterations);
  printf("min-ms=%.3f ms\n", report->min_ms);
  printf("max-ms=%.3f ms\n", report->max_ms);
}

static report_t execute_benchmark(const benchmark_spec_t *spec, int size, int warmup, int iterations, void *env, prepare_fn prepare, run_fn run, validate_fn validate) {
  report_t report;
  report.benchmark = spec->name;
  report.description = spec->description;
  report.size = size;
  report.warmup = warmup;
  report.iterations = iterations;
  report.has_warmup_checksum = 0;
  report.warmup_checksum = 0;
  report.checksum = 0;
  report.total_ms = 0.0;
  report.min_ms = 0.0;
  report.max_ms = 0.0;

  for (int i = 0; i < warmup; ++i) {
    prepare(env);
    run(env);
    report.warmup_checksum = validate(env);
    report.has_warmup_checksum = 1;
  }

  for (int i = 0; i < iterations; ++i) {
    prepare(env);
    double start = omp_get_wtime();
    run(env);
    double end = omp_get_wtime();
    checksum_t checksum = validate(env);
    double elapsed_ms = (end - start) * 1000.0;
    if (i == 0) {
      report.checksum = checksum;
      report.min_ms = elapsed_ms;
      report.max_ms = elapsed_ms;
    } else {
      if (checksum != report.checksum) {
        fprintf(stderr, "benchmark checksum changed across iterations: expected %" PRId64 ", got %" PRId64 "\n", report.checksum, checksum);
        exit(1);
      }
      if (elapsed_ms < report.min_ms) {
        report.min_ms = elapsed_ms;
      }
      if (elapsed_ms > report.max_ms) {
        report.max_ms = elapsed_ms;
      }
    }
    report.total_ms += elapsed_ms;
  }

  return report;
}

static void kernel_fill_i64(int n, int *out) {
#pragma omp parallel for schedule(static)
  for (int i = 0; i < n; ++i) {
    out[i] = (int64_t)i * 3 + 1;
  }
}

static void kernel_fill_3d_i64(int n, int *out) {
#pragma omp parallel for collapse(3) schedule(static)
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      for (int k = 0; k < n; ++k) {
        out[index3(n, i, j, k)] = (int64_t)i * 1000000 + (int64_t)j * 1000 + k;
      }
    }
  }
}

static void kernel_map_i64(int n, const int *left, const int *right, int *out) {
#pragma omp parallel for schedule(static)
  for (int i = 0; i < n; ++i) {
    out[i] = left[i] + (2 * right[i]);
  }
}

static checksum_t kernel_sum_i64(int n, const int *input) {
  checksum_t total = 0;
#pragma omp parallel for reduction(+ : total) schedule(static)
  for (int i = 0; i < n; ++i) {
    total += input[i];
  }
  return total;
}

static checksum_t kernel_dot_i64(int n, const int *left, const int *right) {
  checksum_t total = 0;
#pragma omp parallel for reduction(+ : total) schedule(static)
  for (int i = 0; i < n; ++i) {
    total += left[i] * right[i];
  }
  return total;
}

static void kernel_jacobi_step(int n, const double *prev, double *next) {
  next[0] = prev[0];
  if (n > 1) {
    next[n - 1] = prev[n - 1];
  }
  if (n <= 2) {
    return;
  }
#pragma omp parallel for simd schedule(static)
  for (int i = 1; i < n - 1; ++i) {
    next[i] = (prev[i - 1] + prev[i] + prev[i] + prev[i + 1]) * 0.25;
  }
}

static double *kernel_jacobi_run(int steps, int n, double *current, double *scratch) {
  double *src = current;
  double *dst = scratch;
  for (int step = 0; step < steps; ++step) {
    kernel_jacobi_step(n, src, dst);
    double *tmp = src;
    src = dst;
    dst = tmp;
  }
  return src;
}

static void kernel_nbody(int n, const double *pos_x, const double *pos_y, const double *pos_z, const double *mass, double *acc_x, double *acc_y, double *acc_z) {
#pragma omp parallel for schedule(static)
  for (int i = 0; i < n; ++i) {
    double xi = pos_x[i];
    double yi = pos_y[i];
    double zi = pos_z[i];
    double ax = 0.0;
    double ay = 0.0;
    double az = 0.0;
#pragma omp simd reduction(+ : ax, ay, az)
    for (int j = 0; j < n; ++j) {
      double active = (i == j) ? 0.0 : 1.0;
      double dx = pos_x[j] - xi;
      double dy = pos_y[j] - yi;
      double dz = pos_z[j] - zi;
      double dist_sq = (dx * dx) + (dy * dy) + (dz * dz) + 1.0e-9;
      double inv_dist = 1.0 / sqrt(dist_sq);
      double scale = active * mass[j] * inv_dist * inv_dist * inv_dist;
      ax += dx * scale;
      ay += dy * scale;
      az += dz * scale;
    }
    acc_x[i] = ax;
    acc_y[i] = ay;
    acc_z[i] = az;
  }
}

static void kernel_matmul_i64(int n, const int *left, const int *right, int *out) {
#pragma omp parallel for collapse(2) schedule(static)
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      checksum_t total = 0;
#pragma omp simd reduction(+ : total)
      for (int k = 0; k < n; ++k) {
        total += left[index2(n, i, k)] * right[index2(n, k, j)];
      }
      out[index2(n, i, j)] = total;
    }
  }
}

static void kernel_tiled_matmul_i64(int n, const int *left, const int *right, int *out) {
  int tile = min_int(32, n);
#pragma omp parallel for collapse(2) schedule(static)
  for (int row0 = 0; row0 < n; row0 += tile) {
    for (int col0 = 0; col0 < n; col0 += tile) {
      int row_limit = min_int(n, row0 + tile);
      int col_limit = min_int(n, col0 + tile);
      for (int i = row0; i < row_limit; ++i) {
        for (int j = col0; j < col_limit; ++j) {
          checksum_t total = 0;
#pragma omp simd reduction(+ : total)
          for (int k = 0; k < n; ++k) {
            total += left[index2(n, i, k)] * right[index2(n, k, j)];
          }
          out[index2(n, i, j)] = total;
        }
      }
    }
  }
}

static void kernel_tiled_matmul_i32(int n, const int32_t *left, const int32_t *right, int32_t *out) {
  int tile = min_int(32, n);
#pragma omp parallel for collapse(2) schedule(static)
  for (int row0 = 0; row0 < n; row0 += tile) {
    for (int col0 = 0; col0 < n; col0 += tile) {
      int row_limit = min_int(n, row0 + tile);
      int col_limit = min_int(n, col0 + tile);
      for (int i = row0; i < row_limit; ++i) {
        for (int j = col0; j < col_limit; ++j) {
          checksum_t total = 0;
          for (int k = 0; k < n; ++k) {
            total += (checksum_t)left[index2(n, i, k)] * (checksum_t)right[index2(n, k, j)];
          }
          out[index2(n, i, j)] = (int32_t)total;
        }
      }
    }
  }
}

static void kernel_tiled_matmul_i32_vec(int n, const int32_t *left, const int32_t *right, int32_t *out) {
  int tile = min_int(32, n);
#pragma omp parallel for collapse(2) schedule(static)
  for (int row0 = 0; row0 < n; row0 += tile) {
    for (int col0 = 0; col0 < n; col0 += tile) {
      int row_limit = min_int(n, row0 + tile);
      int col_limit = min_int(n, col0 + tile);
      for (int i = row0; i < row_limit; ++i) {
        for (int j = col0; j < col_limit; ++j) {
          checksum_t total = 0;
#pragma omp simd reduction(+ : total)
          for (int k = 0; k < n; ++k) {
            total += (checksum_t)left[index2(n, i, k)] * (checksum_t)right[index2(n, k, j)];
          }
          out[index2(n, i, j)] = (int32_t)total;
        }
      }
    }
  }
}

static void kernel_matmul_double_scalar(int n, const double *left, const double *right, double *out) {
#pragma omp parallel for collapse(2) schedule(static)
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      double total = 0.0;
      for (int k = 0; k < n; ++k) {
        total += left[index2(n, i, k)] * right[index2(n, k, j)];
      }
      out[index2(n, i, j)] = total;
    }
  }
}

static void kernel_tiled_matmul_double_scalar(int n, const double *left, const double *right, double *out) {
  int tile = min_int(32, n);
#pragma omp parallel for collapse(2) schedule(static)
  for (int row0 = 0; row0 < n; row0 += tile) {
    for (int col0 = 0; col0 < n; col0 += tile) {
      int row_limit = min_int(n, row0 + tile);
      int col_limit = min_int(n, col0 + tile);
      for (int i = row0; i < row_limit; ++i) {
        for (int j = col0; j < col_limit; ++j) {
          double total = 0.0;
          for (int k = 0; k < n; ++k) {
            total += left[index2(n, i, k)] * right[index2(n, k, j)];
          }
          out[index2(n, i, j)] = total;
        }
      }
    }
  }
}

static void kernel_tiled_matmul_double_vec(int n, const double *left, const double *right, double *out) {
  int tile = min_int(32, n);
#pragma omp parallel for collapse(2) schedule(static)
  for (int row0 = 0; row0 < n; row0 += tile) {
    for (int col0 = 0; col0 < n; col0 += tile) {
      int row_limit = min_int(n, row0 + tile);
      int col_limit = min_int(n, col0 + tile);
      for (int i = row0; i < row_limit; ++i) {
        for (int j = col0; j < col_limit; ++j) {
          double total = 0.0;
#pragma omp simd reduction(+ : total)
          for (int k = 0; k < n; ++k) {
            total += left[index2(n, i, k)] * right[index2(n, k, j)];
          }
          out[index2(n, i, j)] = total;
        }
      }
    }
  }
}

static void kernel_wavefront_edit_distance(int rows, int cols, const int *left, const int *right, int *dp) {
  int table_cols = cols + 1;
#pragma omp parallel
  {
#pragma omp for schedule(static)
    for (int i = 0; i <= rows; ++i) {
      dp[(size_t)i * (size_t)table_cols] = i;
    }
#pragma omp for schedule(static)
    for (int j = 1; j <= cols; ++j) {
      dp[j] = j;
    }
    for (int diag = 2; diag <= rows + cols; ++diag) {
      int i_start = max_int(1, diag - cols);
      int i_end = min_int(rows, diag - 1);
#pragma omp for schedule(static)
      for (int i = i_start; i <= i_end; ++i) {
        int j = diag - i;
        size_t row_base = (size_t)i * (size_t)table_cols;
        size_t prev_row_base = (size_t)(i - 1) * (size_t)table_cols;
        int64_t deletion = dp[prev_row_base + j];
        int64_t insertion = dp[row_base + (j - 1)];
        int64_t substitution = dp[prev_row_base + (j - 1)];
        int64_t cost = (left[i - 1] == right[j - 1]) ? 0 : 1;
        int64_t cell = min_i64(deletion + 1, min_i64(insertion + 1, substitution + cost));
        dp[row_base + j] = cell;
      }
    }
  }
}

static void kernel_red_black_stencil(int n, const int *src, int *out) {
#pragma omp parallel
  {
#pragma omp for collapse(2) schedule(static)
    for (int i = 0; i < n; ++i) {
      for (int j = 0; j < n; ++j) {
        out[index2(n, i, j)] = src[index2(n, i, j)];
      }
    }
#pragma omp for collapse(2) schedule(static)
    for (int i = 1; i < n - 1; ++i) {
      for (int j = 1; j < n - 1; ++j) {
        if (((i + j) & 1) == 0) {
          int64_t center = out[index2(n, i, j)];
          int64_t up = out[index2(n, i - 1, j)];
          int64_t down = out[index2(n, i + 1, j)];
          int64_t left = out[index2(n, i, j - 1)];
          int64_t right = out[index2(n, i, j + 1)];
          out[index2(n, i, j)] = (center + up + down + left + right) / 5;
        }
      }
    }
#pragma omp for collapse(2) schedule(static)
    for (int i = 1; i < n - 1; ++i) {
      for (int j = 1; j < n - 1; ++j) {
        if (((i + j) & 1) != 0) {
          int64_t center = out[index2(n, i, j)];
          int64_t up = out[index2(n, i - 1, j)];
          int64_t down = out[index2(n, i + 1, j)];
          int64_t left = out[index2(n, i, j - 1)];
          int64_t right = out[index2(n, i, j + 1)];
          out[index2(n, i, j)] = (center + up + down + left + right) / 5;
        }
      }
    }
  }
}

static void kernel_normalize_3phase(int n, const double *input, double *output) {
  double total = 0.0;
#pragma omp parallel for reduction(+ : total) schedule(static)
  for (int i = 0; i < n; ++i) {
    total += input[i];
  }
  double mean = total / (double)n;

  double sq_total = 0.0;
#pragma omp parallel for reduction(+ : sq_total) schedule(static)
  for (int i = 0; i < n; ++i) {
    double delta = input[i] - mean;
    sq_total += delta * delta;
  }
  double variance = sq_total / (double)n;
  double inv_std = variance <= 0.0 ? 0.0 : 1.0 / sqrt(variance);

#pragma omp parallel for schedule(static)
  for (int i = 0; i < n; ++i) {
    output[i] = (input[i] - mean) * inv_std;
  }
}

static int clamp_index(int n, int value) {
  if (value < 0) {
    return 0;
  }
  if (value >= n) {
    return n - 1;
  }
  return value;
}

static void kernel_separable_blur(int n, const int *src, int *tmp, int *out) {
#pragma omp parallel
  {
#pragma omp for collapse(2) schedule(static)
    for (int i = 0; i < n; ++i) {
      for (int j = 0; j < n; ++j) {
        int jl = clamp_index(n, j - 1);
        int jr = clamp_index(n, j + 1);
        int64_t left = src[index2(n, i, jl)];
        int64_t center = src[index2(n, i, j)];
        int64_t right = src[index2(n, i, jr)];
        tmp[index2(n, i, j)] = (left + center + right) / 3;
      }
    }
#pragma omp for collapse(2) schedule(static)
    for (int i = 0; i < n; ++i) {
      for (int j = 0; j < n; ++j) {
        int iu = clamp_index(n, i - 1);
        int id = clamp_index(n, i + 1);
        int64_t up = tmp[index2(n, iu, j)];
        int64_t center = tmp[index2(n, i, j)];
        int64_t down = tmp[index2(n, id, j)];
        out[index2(n, i, j)] = (up + center + down) / 3;
      }
    }
  }
}

static void prepare_jacobi(void *env) {
  JacobiEnv *state = (JacobiEnv *)env;
  memcpy(state->current, state->input, (size_t)state->n * sizeof(double));
}

static void kernel_wavefront_lcs(int rows, int cols, const int *left, const int *right, int *dp) {
  int table_cols = cols + 1;
#pragma omp parallel
  {
#pragma omp for schedule(static)
    for (int i = 0; i <= rows; ++i) {
      dp[(size_t)i * (size_t)table_cols] = 0;
    }
#pragma omp for schedule(static)
    for (int j = 1; j <= cols; ++j) {
      dp[j] = 0;
    }
    for (int diag = 2; diag <= rows + cols; ++diag) {
      int i_start = max_int(1, diag - cols);
      int i_end = min_int(rows, diag - 1);
#pragma omp for schedule(static)
      for (int i = i_start; i <= i_end; ++i) {
        int j = diag - i;
        size_t row_base = (size_t)i * (size_t)table_cols;
        size_t prev_row_base = (size_t)(i - 1) * (size_t)table_cols;
        if (left[i - 1] == right[j - 1]) {
          dp[row_base + j] = dp[prev_row_base + (j - 1)] + 1;
        } else {
          int from_up   = dp[prev_row_base + j];
          int from_left = dp[row_base + (j - 1)];
          dp[row_base + j] = max_int(from_up, from_left);
        }
      }
    }
  }
}

static void kernel_jacobi_2d_step(int n, const double *prev, double *next) {
#pragma omp parallel for collapse(2) schedule(static)
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      size_t idx = index2(n, i, j);
      double center = prev[idx];
      if (i > 0 && i < n - 1 && j > 0 && j < n - 1) {
        double up    = prev[index2(n, i - 1, j)];
        double down  = prev[index2(n, i + 1, j)];
        double lv    = prev[index2(n, i, j - 1)];
        double rv    = prev[index2(n, i, j + 1)];
        next[idx] = (center + up + down + lv + rv) * 0.2;
      } else {
        next[idx] = center;
      }
    }
  }
}

static void kernel_tiled_3d_map(int n, const int *left, const int *right, int *out) {
#pragma omp parallel for collapse(3) schedule(static)
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < n; ++j)
      for (int k = 0; k < n; ++k) {
        size_t idx = index3(n, i, j, k);
        out[idx] = left[idx] + 2 * right[idx];
      }
}

static void prepare_jacobi_2d(void *env) {
  Jacobi2DEnv *state = (Jacobi2DEnv *)env;
  memcpy(state->current, state->input, (size_t)state->n * (size_t)state->n * sizeof(double));
}

static void run_wavefront_lcs(void *env) {
  LCSEnv *state = (LCSEnv *)env;
  kernel_wavefront_lcs(state->rows, state->cols, state->left, state->right, state->dp);
}

static checksum_t validate_wavefront_lcs(void *env) {
  LCSEnv *state = (LCSEnv *)env;
  return loom_bench_sample_2d_int(state->dp, state->cols + 1, state->cols + 1);
}

static void run_jacobi_2d(void *env) {
  Jacobi2DEnv *state = (Jacobi2DEnv *)env;
  double *cur = state->current;
  double *nxt = state->scratch;
  for (int s = 0; s < state->steps; ++s) {
    kernel_jacobi_2d_step(state->n, cur, nxt);
    double *tmp = cur; cur = nxt; nxt = tmp;
  }
  state->final_buffer = cur;
}

static checksum_t validate_jacobi_2d(void *env) {
  Jacobi2DEnv *state = (Jacobi2DEnv *)env;
  return loom_bench_sample_2d_double(state->final_buffer, state->n, state->n);
}

static void run_tiled_3d_map(void *env) {
  Volume3DMapEnv *state = (Volume3DMapEnv *)env;
  kernel_tiled_3d_map(state->n, state->left, state->right, state->out);
}

static checksum_t validate_tiled_3d_map(void *env) {
  Volume3DMapEnv *state = (Volume3DMapEnv *)env;
  return loom_bench_sample_3d_int(state->out, state->n, state->n, state->n);
}

static void run_fill(void *env) {
  FillEnv *state = (FillEnv *)env;
  kernel_fill_i64(state->n, state->out);
}

static checksum_t validate_fill(void *env) {
  FillEnv *state = (FillEnv *)env;
  return loom_bench_sample_1d_int(state->out, state->n);
}

static void run_fill_3d(void *env) {
  Fill3DEnv *state = (Fill3DEnv *)env;
  kernel_fill_3d_i64(state->n, state->out);
}

static checksum_t validate_fill_3d(void *env) {
  Fill3DEnv *state = (Fill3DEnv *)env;
  return loom_bench_sample_3d_int(state->out, state->n, state->n, state->n);
}

static void run_map(void *env) {
  BinaryI64Env *state = (BinaryI64Env *)env;
  kernel_map_i64(state->n, state->left, state->right, state->out);
}

static checksum_t validate_map(void *env) {
  BinaryI64Env *state = (BinaryI64Env *)env;
  return loom_bench_sample_1d_int(state->out, state->n);
}

static void run_sum(void *env) {
  ReduceI64Env *state = (ReduceI64Env *)env;
  state->value = kernel_sum_i64(state->n, state->input);
}

static checksum_t validate_sum(void *env) {
  ReduceI64Env *state = (ReduceI64Env *)env;
  return state->value;
}

static void run_dot(void *env) {
  DotEnv *state = (DotEnv *)env;
  state->value = kernel_dot_i64(state->n, state->left, state->right);
}

static checksum_t validate_dot(void *env) {
  DotEnv *state = (DotEnv *)env;
  return state->value;
}

static void run_jacobi(void *env) {
  JacobiEnv *state = (JacobiEnv *)env;
  state->final_buffer = kernel_jacobi_run(state->steps, state->n, state->current, state->scratch);
}

static checksum_t validate_jacobi(void *env) {
  JacobiEnv *state = (JacobiEnv *)env;
  return loom_bench_sample_1d_double(state->final_buffer, state->n);
}

static void run_nbody(void *env) {
  NBodyEnv *state = (NBodyEnv *)env;
  kernel_nbody(state->n, state->pos_x, state->pos_y, state->pos_z, state->mass, state->acc_x, state->acc_y, state->acc_z);
}

static checksum_t validate_nbody(void *env) {
  NBodyEnv *state = (NBodyEnv *)env;
  return loom_bench_sample_double_triple(state->acc_x, state->acc_y, state->acc_z, state->n);
}

static void run_matmul_i64(void *env) {
  MatrixI64Env *state = (MatrixI64Env *)env;
  kernel_matmul_i64(state->n, state->left, state->right, state->out);
}

static checksum_t validate_matmul_i64(void *env) {
  MatrixI64Env *state = (MatrixI64Env *)env;
  return loom_bench_sample_2d_int(state->out, state->n, state->n);
}

static void run_tiled_matmul_i64(void *env) {
  MatrixI64Env *state = (MatrixI64Env *)env;
  kernel_tiled_matmul_i64(state->n, state->left, state->right, state->out);
}

static void run_int32_tiled_matmul_scalar(void *env) {
  MatrixI32Env *state = (MatrixI32Env *)env;
  kernel_tiled_matmul_i32(state->n, state->left, state->right, state->out);
}

static checksum_t validate_matmul_i32(void *env) {
  MatrixI32Env *state = (MatrixI32Env *)env;
  return loom_bench_sample_2d_int32(state->out, state->n, state->n);
}

static void run_int32_tiled_matmul_vec(void *env) {
  MatrixI32Env *state = (MatrixI32Env *)env;
  kernel_tiled_matmul_i32_vec(state->n, state->left, state->right, state->out);
}

static void run_double_matmul_scalar(void *env) {
  DoubleMatrixEnv *state = (DoubleMatrixEnv *)env;
  kernel_matmul_double_scalar(state->n, state->left, state->right, state->out);
}

static checksum_t validate_double_matmul(void *env) {
  DoubleMatrixEnv *state = (DoubleMatrixEnv *)env;
  return loom_bench_sample_2d_double(state->out, state->n, state->n);
}

static void run_double_tiled_matmul_scalar(void *env) {
  DoubleMatrixEnv *state = (DoubleMatrixEnv *)env;
  kernel_tiled_matmul_double_scalar(state->n, state->left, state->right, state->out);
}

static void run_double_tiled_matmul_vec(void *env) {
  DoubleMatrixEnv *state = (DoubleMatrixEnv *)env;
  kernel_tiled_matmul_double_vec(state->n, state->left, state->right, state->out);
}

static void run_double_tiled_matmul_vec_newapi(void *env) {
  DoubleMatrixEnv *state = (DoubleMatrixEnv *)env;
  kernel_tiled_matmul_double_vec(state->n, state->left, state->right, state->out);
}

static void run_wavefront(void *env) {
  WavefrontEnv *state = (WavefrontEnv *)env;
  kernel_wavefront_edit_distance(state->rows, state->cols, state->left, state->right, state->dp);
}

static checksum_t validate_wavefront(void *env) {
  WavefrontEnv *state = (WavefrontEnv *)env;
  return loom_bench_sample_2d_int(state->dp, state->cols + 1, state->cols + 1);
}

static void run_red_black(void *env) {
  ImageEnv *state = (ImageEnv *)env;
  kernel_red_black_stencil(state->n, state->input, state->output);
}

static checksum_t validate_red_black(void *env) {
  ImageEnv *state = (ImageEnv *)env;
  return loom_bench_sample_2d_int(state->output, state->n, state->n);
}

static void run_normalize(void *env) {
  NormalizeEnv *state = (NormalizeEnv *)env;
  kernel_normalize_3phase(state->n, state->input, state->output);
}

static checksum_t validate_normalize(void *env) {
  NormalizeEnv *state = (NormalizeEnv *)env;
  return loom_bench_sample_1d_double(state->output, state->n);
}

static void run_separable_blur(void *env) {
  BlurEnv *state = (BlurEnv *)env;
  kernel_separable_blur(state->n, state->input, state->scratch, state->output);
}

static checksum_t validate_separable_blur(void *env) {
  BlurEnv *state = (BlurEnv *)env;
  return loom_bench_sample_2d_int(state->output, state->n, state->n);
}

static report_t run_selected(const benchmark_spec_t *spec, int size, int warmup, int iterations) {
  switch (spec->kind) {
    case BENCH_FILL: {
      FillEnv env = {size, loom_bench_alloc_ints((size_t)size)};
      report_t report = execute_benchmark(spec, size, warmup, iterations, &env, prepare_noop, run_fill, validate_fill);
      free(env.out);
      return report;
    }
    case BENCH_FILL_3D: {
      Fill3DEnv env = {size, loom_bench_alloc_ints((size_t)size * (size_t)size * (size_t)size)};
      report_t report = execute_benchmark(spec, size, warmup, iterations, &env, prepare_noop, run_fill_3d, validate_fill_3d);
      free(env.out);
      return report;
    }
    case BENCH_MAP: {
      BinaryI64Env env = {size, loom_bench_alloc_ints((size_t)size), loom_bench_alloc_ints((size_t)size), loom_bench_alloc_ints((size_t)size)};
      loom_bench_fill_seeded_int(env.left, (size_t)size, 17, 11);
      loom_bench_fill_seeded_int(env.right, (size_t)size, 31, 7);
      report_t report = execute_benchmark(spec, size, warmup, iterations, &env, prepare_noop, run_map, validate_map);
      free(env.left);
      free(env.right);
      free(env.out);
      return report;
    }
    case BENCH_SUM: {
      ReduceI64Env env = {size, loom_bench_alloc_ints((size_t)size), 0};
      loom_bench_fill_seeded_int(env.input, (size_t)size, 17, 11);
      report_t report = execute_benchmark(spec, size, warmup, iterations, &env, prepare_noop, run_sum, validate_sum);
      free(env.input);
      return report;
    }
    case BENCH_DOT: {
      DotEnv env = {size, loom_bench_alloc_ints((size_t)size), loom_bench_alloc_ints((size_t)size), 0};
      loom_bench_fill_seeded_int(env.left, (size_t)size, 17, 11);
      loom_bench_fill_seeded_int(env.right, (size_t)size, 31, 7);
      report_t report = execute_benchmark(spec, size, warmup, iterations, &env, prepare_noop, run_dot, validate_dot);
      free(env.left);
      free(env.right);
      return report;
    }
    case BENCH_JACOBI_1D:
    case BENCH_VERIFIED_JACOBI_1D: {
      JacobiEnv env;
      env.n = size;
      env.steps = 50;
      env.input = loom_bench_alloc_doubles((size_t)size);
      env.current = loom_bench_alloc_doubles((size_t)size);
      env.scratch = loom_bench_alloc_doubles((size_t)size);
      env.final_buffer = NULL;
      loom_bench_fill_seeded_double(env.input, (size_t)size, 17, 11);
      loom_bench_fill_seeded_double(env.current, (size_t)size, 17, 11);
      prepare_fn prepare = prepare_jacobi;
      run_fn run = run_jacobi;
      validate_fn validate = validate_jacobi;
      report_t report = execute_benchmark(spec, size, warmup, iterations, &env, prepare, run, validate);
      free(env.input);
      free(env.current);
      free(env.scratch);
      return report;
    }
    case BENCH_NBODY: {
      NBodyEnv env;
      env.n = size;
      env.pos_x = loom_bench_alloc_doubles((size_t)size);
      env.pos_y = loom_bench_alloc_doubles((size_t)size);
      env.pos_z = loom_bench_alloc_doubles((size_t)size);
      env.mass = loom_bench_alloc_doubles((size_t)size);
      env.acc_x = loom_bench_alloc_doubles((size_t)size);
      env.acc_y = loom_bench_alloc_doubles((size_t)size);
      env.acc_z = loom_bench_alloc_doubles((size_t)size);
      loom_bench_fill_seeded_signed_double(env.pos_x, (size_t)size, 17, 11);
      loom_bench_fill_seeded_signed_double(env.pos_y, (size_t)size, 31, 7);
      loom_bench_fill_seeded_signed_double(env.pos_z, (size_t)size, 43, 3);
      loom_bench_fill_seeded_positive_double(env.mass, (size_t)size, 29, 5);
      report_t report = execute_benchmark(spec, size, warmup, iterations, &env, prepare_noop, run_nbody, validate_nbody);
      free(env.pos_x);
      free(env.pos_y);
      free(env.pos_z);
      free(env.mass);
      free(env.acc_x);
      free(env.acc_y);
      free(env.acc_z);
      return report;
    }
    case BENCH_MATMUL: {
      MatrixI64Env env = {size, loom_bench_alloc_ints((size_t)size * (size_t)size), loom_bench_alloc_ints((size_t)size * (size_t)size), loom_bench_alloc_ints((size_t)size * (size_t)size)};
      loom_bench_fill_seeded_int(env.left, (size_t)size * (size_t)size, 17, 11);
      loom_bench_fill_seeded_int(env.right, (size_t)size * (size_t)size, 31, 7);
      report_t report = execute_benchmark(spec, size, warmup, iterations, &env, prepare_noop, run_matmul_i64, validate_matmul_i64);
      free(env.left);
      free(env.right);
      free(env.out);
      return report;
    }
    case BENCH_TILED_MATMUL: {
      MatrixI64Env env = {size, loom_bench_alloc_ints((size_t)size * (size_t)size), loom_bench_alloc_ints((size_t)size * (size_t)size), loom_bench_alloc_ints((size_t)size * (size_t)size)};
      loom_bench_fill_seeded_int(env.left, (size_t)size * (size_t)size, 17, 11);
      loom_bench_fill_seeded_int(env.right, (size_t)size * (size_t)size, 31, 7);
      report_t report = execute_benchmark(spec, size, warmup, iterations, &env, prepare_noop, run_tiled_matmul_i64, validate_matmul_i64);
      free(env.left);
      free(env.right);
      free(env.out);
      return report;
    }
    case BENCH_INT32_TILED_MATMUL_SCALAR: {
      MatrixI32Env env = {size, loom_bench_alloc_int32s((size_t)size * (size_t)size), loom_bench_alloc_int32s((size_t)size * (size_t)size), loom_bench_alloc_int32s((size_t)size * (size_t)size)};
      loom_bench_fill_seeded_int32(env.left, (size_t)size * (size_t)size, 17, 11);
      loom_bench_fill_seeded_int32(env.right, (size_t)size * (size_t)size, 31, 7);
      report_t report = execute_benchmark(spec, size, warmup, iterations, &env, prepare_noop, run_int32_tiled_matmul_scalar, validate_matmul_i32);
      free(env.left);
      free(env.right);
      free(env.out);
      return report;
    }
    case BENCH_INT32_TILED_MATMUL_VEC: {
      MatrixI32Env env = {size, loom_bench_alloc_int32s((size_t)size * (size_t)size), loom_bench_alloc_int32s((size_t)size * (size_t)size), loom_bench_alloc_int32s((size_t)size * (size_t)size)};
      loom_bench_fill_seeded_int32(env.left, (size_t)size * (size_t)size, 17, 11);
      loom_bench_fill_seeded_int32(env.right, (size_t)size * (size_t)size, 31, 7);
      report_t report = execute_benchmark(spec, size, warmup, iterations, &env, prepare_noop, run_int32_tiled_matmul_vec, validate_matmul_i32);
      free(env.left);
      free(env.right);
      free(env.out);
      return report;
    }
    case BENCH_DOUBLE_MATMUL_SCALAR: {
      DoubleMatrixEnv env = {size, loom_bench_alloc_doubles((size_t)size * (size_t)size), loom_bench_alloc_doubles((size_t)size * (size_t)size), loom_bench_alloc_doubles((size_t)size * (size_t)size)};
      loom_bench_fill_seeded_double(env.left, (size_t)size * (size_t)size, 17, 11);
      loom_bench_fill_seeded_double(env.right, (size_t)size * (size_t)size, 31, 7);
      report_t report = execute_benchmark(spec, size, warmup, iterations, &env, prepare_noop, run_double_matmul_scalar, validate_double_matmul);
      free(env.left);
      free(env.right);
      free(env.out);
      return report;
    }
    case BENCH_DOUBLE_TILED_MATMUL_SCALAR: {
      DoubleMatrixEnv env = {size, loom_bench_alloc_doubles((size_t)size * (size_t)size), loom_bench_alloc_doubles((size_t)size * (size_t)size), loom_bench_alloc_doubles((size_t)size * (size_t)size)};
      loom_bench_fill_seeded_double(env.left, (size_t)size * (size_t)size, 17, 11);
      loom_bench_fill_seeded_double(env.right, (size_t)size * (size_t)size, 31, 7);
      report_t report = execute_benchmark(spec, size, warmup, iterations, &env, prepare_noop, run_double_tiled_matmul_scalar, validate_double_matmul);
      free(env.left);
      free(env.right);
      free(env.out);
      return report;
    }
    case BENCH_DOUBLE_TILED_MATMUL_VEC: {
      DoubleMatrixEnv env = {size, loom_bench_alloc_doubles((size_t)size * (size_t)size), loom_bench_alloc_doubles((size_t)size * (size_t)size), loom_bench_alloc_doubles((size_t)size * (size_t)size)};
      loom_bench_fill_seeded_double(env.left, (size_t)size * (size_t)size, 17, 11);
      loom_bench_fill_seeded_double(env.right, (size_t)size * (size_t)size, 31, 7);
      report_t report = execute_benchmark(spec, size, warmup, iterations, &env, prepare_noop, run_double_tiled_matmul_vec, validate_double_matmul);
      free(env.left);
      free(env.right);
      free(env.out);
      return report;
    }
    case BENCH_DOUBLE_TILED_MATMUL_VEC_NEWAPI: {
      DoubleMatrixEnv env = {size, loom_bench_alloc_doubles((size_t)size * (size_t)size), loom_bench_alloc_doubles((size_t)size * (size_t)size), loom_bench_alloc_doubles((size_t)size * (size_t)size)};
      loom_bench_fill_seeded_double(env.left, (size_t)size * (size_t)size, 17, 11);
      loom_bench_fill_seeded_double(env.right, (size_t)size * (size_t)size, 31, 7);
      report_t report = execute_benchmark(spec, size, warmup, iterations, &env, prepare_noop, run_double_tiled_matmul_vec_newapi, validate_double_matmul);
      free(env.left);
      free(env.right);
      free(env.out);
      return report;
    }
    case BENCH_WAVEFRONT_EDIT_DISTANCE: {
      WavefrontEnv env;
      env.rows = size;
      env.cols = size;
      env.left = loom_bench_alloc_ints((size_t)size);
      env.right = loom_bench_alloc_ints((size_t)size);
      env.dp = loom_bench_alloc_ints((size_t)(size + 1) * (size_t)(size + 1));
      loom_bench_fill_seeded_int(env.left, (size_t)size, 17, 11);
      loom_bench_fill_seeded_int(env.right, (size_t)size, 31, 7);
      report_t report = execute_benchmark(spec, size, warmup, iterations, &env, prepare_noop, run_wavefront, validate_wavefront);
      free(env.left);
      free(env.right);
      free(env.dp);
      return report;
    }
    case BENCH_RED_BLACK_STENCIL: {
      ImageEnv env = {size, loom_bench_alloc_ints((size_t)size * (size_t)size), loom_bench_alloc_ints((size_t)size * (size_t)size)};
      loom_bench_fill_seeded_int(env.input, (size_t)size * (size_t)size, 17, 11);
      report_t report = execute_benchmark(spec, size, warmup, iterations, &env, prepare_noop, run_red_black, validate_red_black);
      free(env.input);
      free(env.output);
      return report;
    }
    case BENCH_NORMALIZE_3PHASE: {
      NormalizeEnv env = {size, loom_bench_alloc_doubles((size_t)size), loom_bench_alloc_doubles((size_t)size)};
      loom_bench_fill_seeded_double(env.input, (size_t)size, 17, 11);
      report_t report = execute_benchmark(spec, size, warmup, iterations, &env, prepare_noop, run_normalize, validate_normalize);
      free(env.input);
      free(env.output);
      return report;
    }
    case BENCH_SEPARABLE_BLUR: {
      BlurEnv env = {size, loom_bench_alloc_ints((size_t)size * (size_t)size), loom_bench_alloc_ints((size_t)size * (size_t)size), loom_bench_alloc_ints((size_t)size * (size_t)size)};
      loom_bench_fill_seeded_int(env.input, (size_t)size * (size_t)size, 17, 11);
      report_t report = execute_benchmark(spec, size, warmup, iterations, &env, prepare_noop, run_separable_blur, validate_separable_blur);
      free(env.input);
      free(env.scratch);
      free(env.output);
      return report;
    }
    case BENCH_WAVEFRONT_LCS: {
      LCSEnv env;
      env.rows = size;
      env.cols = size;
      env.left = loom_bench_alloc_ints((size_t)size);
      env.right = loom_bench_alloc_ints((size_t)size);
      env.dp = loom_bench_alloc_ints((size_t)(size + 1) * (size_t)(size + 1));
      loom_bench_fill_seeded_int(env.left, (size_t)size, 17, 11);
      loom_bench_fill_seeded_int(env.right, (size_t)size, 31, 7);
      report_t report = execute_benchmark(spec, size, warmup, iterations, &env, prepare_noop, run_wavefront_lcs, validate_wavefront_lcs);
      free(env.left);
      free(env.right);
      free(env.dp);
      return report;
    }
    case BENCH_JACOBI_2D: {
      int steps = 50;
      size_t cells = (size_t)size * (size_t)size;
      Jacobi2DEnv env;
      env.n = size;
      env.steps = steps;
      env.input = loom_bench_alloc_doubles(cells);
      env.current = loom_bench_alloc_doubles(cells);
      env.scratch = loom_bench_alloc_doubles(cells);
      env.final_buffer = NULL;
      loom_bench_fill_seeded_double(env.input, cells, 17, 11);
      memcpy(env.current, env.input, cells * sizeof(double));
      report_t report = execute_benchmark(spec, size, warmup, iterations, &env, prepare_jacobi_2d, run_jacobi_2d, validate_jacobi_2d);
      free(env.input);
      free(env.current);
      free(env.scratch);
      return report;
    }
    case BENCH_TILED_3D_MAP: {
      size_t cells = (size_t)size * (size_t)size * (size_t)size;
      Volume3DMapEnv env;
      env.n = size;
      env.left  = loom_bench_alloc_ints(cells);
      env.right = loom_bench_alloc_ints(cells);
      env.out   = loom_bench_alloc_ints(cells);
      loom_bench_fill_seeded_int(env.left,  cells, 17, 11);
      loom_bench_fill_seeded_int(env.right, cells, 31, 7);
      report_t report = execute_benchmark(spec, size, warmup, iterations, &env, prepare_noop, run_tiled_3d_map, validate_tiled_3d_map);
      free(env.left);
      free(env.right);
      free(env.out);
      return report;
    }
  }

  fprintf(stderr, "unknown benchmark kind\n");
  exit(1);
}

int main(int argc, char **argv) {
  config_t cfg = parse_args(argv[0], argc, argv);

  if (cfg.list_only) {
    print_benchmark_table();
    return 0;
  }

  if (cfg.benchmark_name == NULL) {
    die_usage(argv[0], "missing benchmark name");
  }

  const benchmark_spec_t *spec = lookup_spec(cfg.benchmark_name);
  if (spec == NULL) {
    fprintf(stderr, "unknown benchmark: %s\n\n", cfg.benchmark_name);
    usage(argv[0]);
    return 1;
  }

  int size = cfg.size > 0 ? cfg.size : spec->default_size;
  report_t report = run_selected(spec, size, cfg.warmup, cfg.iterations);
  print_report(&report);
  return 0;
}
