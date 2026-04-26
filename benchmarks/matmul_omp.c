#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include "c/loom_benchmark_helpers.h"

#ifdef _OPENMP
#include <omp.h>
#endif

typedef enum {
  BENCH_NON_TILED,
  BENCH_TILED
} benchmark_kind;

typedef struct {
  benchmark_kind kind;
  const char *name;
  int size;
  int warmup;
  int iterations;
  int tile;
} config_t;

static void usage(const char *prog) {
  fprintf(stderr,
          "Usage: %s BENCHMARK [--size N] [--warmup N] [--iterations N] [--tile N]\n"
          "  BENCHMARK: double-matmul-omp | double-tiled-matmul-omp\n",
          prog);
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

static config_t parse_args(int argc, char **argv) {
  config_t cfg;
  cfg.kind = BENCH_NON_TILED;
  cfg.name = NULL;
  cfg.size = 256;
  cfg.warmup = 1;
  cfg.iterations = 5;
  cfg.tile = 32;

  if (argc < 2) {
    usage(argv[0]);
    exit(1);
  }

  if (strcmp(argv[1], "double-matmul-omp") == 0) {
    cfg.kind = BENCH_NON_TILED;
    cfg.name = "double-matmul-omp";
  } else if (strcmp(argv[1], "double-tiled-matmul-omp") == 0) {
    cfg.kind = BENCH_TILED;
    cfg.name = "double-tiled-matmul-omp";
  } else {
    fprintf(stderr, "unknown benchmark: %s\n", argv[1]);
    usage(argv[0]);
    exit(1);
  }

  for (int i = 2; i < argc; ++i) {
    if (strcmp(argv[i], "--size") == 0 && i + 1 < argc) {
      cfg.size = parse_positive("size", argv[++i]);
    } else if (strcmp(argv[i], "--warmup") == 0 && i + 1 < argc) {
      cfg.warmup = parse_positive("warmup", argv[++i]);
    } else if (strcmp(argv[i], "--iterations") == 0 && i + 1 < argc) {
      cfg.iterations = parse_positive("iterations", argv[++i]);
    } else if (strcmp(argv[i], "--tile") == 0 && i + 1 < argc) {
      cfg.tile = parse_positive("tile", argv[++i]);
    } else {
      fprintf(stderr, "unrecognized argument: %s\n", argv[i]);
      usage(argv[0]);
      exit(1);
    }
  }

  return cfg;
}

static void matmul_non_tiled(int n, const double *restrict left,
                             const double *restrict right,
                             double *restrict out) {
  #pragma omp parallel for schedule(static)
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      double sum = 0.0;
      #pragma omp simd reduction(+ : sum)
      for (int k = 0; k < n; ++k) {
        sum += left[i * n + k] * right[k * n + j];
      }
      out[i * n + j] = sum;
    }
  }
}

static void matmul_tiled(int n, int tile, const double *restrict left,
                         const double *restrict right, double *restrict out) {
  #pragma omp parallel for collapse(2) schedule(static)
  for (int row0 = 0; row0 < n; row0 += tile) {
    for (int col0 = 0; col0 < n; col0 += tile) {
      int row_limit = row0 + tile < n ? row0 + tile : n;
      int col_limit = col0 + tile < n ? col0 + tile : n;
      for (int i = row0; i < row_limit; ++i) {
        for (int j = col0; j < col_limit; ++j) {
          double sum = 0.0;
          #pragma omp simd reduction(+ : sum)
          for (int k = 0; k < n; ++k) {
            sum += left[i * n + k] * right[k * n + j];
          }
          out[i * n + j] = sum;
        }
      }
    }
  }
}

static void run_once(const config_t *cfg, const double *left, const double *right,
                     double *out) {
  if (cfg->kind == BENCH_NON_TILED) {
    matmul_non_tiled(cfg->size, left, right, out);
  } else {
    matmul_tiled(cfg->size, cfg->tile, left, right, out);
  }
}

int main(int argc, char **argv) {
  config_t cfg = parse_args(argc, argv);
  size_t elems = (size_t)cfg.size * (size_t)cfg.size;
  double *left = loom_bench_alloc_doubles(elems);
  double *right = loom_bench_alloc_doubles(elems);
  double *out = loom_bench_alloc_doubles(elems);

  loom_bench_fill_seeded_double(left, elems, 17, 11);
  loom_bench_fill_seeded_double(right, elems, 31, 7);

  int64_t warmup_checksum = 0;
  for (int i = 0; i < cfg.warmup; ++i) {
    run_once(&cfg, left, right, out);
    warmup_checksum = loom_bench_sample_2d_double(out, (size_t)cfg.size, (size_t)cfg.size);
  }

  double total_ms = 0.0;
  double min_ms = 0.0;
  double max_ms = 0.0;
  loom_bench_checksum_state state;
  loom_bench_checksum_state_init(&state);

  for (int i = 0; i < cfg.iterations; ++i) {
    double start = loom_bench_now_seconds();
    run_once(&cfg, left, right, out);
    double end = loom_bench_now_seconds();
    int64_t current = loom_bench_sample_2d_double(out, (size_t)cfg.size, (size_t)cfg.size);
    double elapsed_ms = (end - start) * 1000.0;

    if (i == 0) {
      min_ms = elapsed_ms;
      max_ms = elapsed_ms;
    }

    if (!loom_bench_checksum_state_update(&state, current)) {
      fprintf(stderr, "checksum changed across iterations: expected %lld got %lld\n",
              (long long)state.expected, (long long)current);
      loom_bench_free(left);
      loom_bench_free(right);
      loom_bench_free(out);
      return 1;
    } else if (i > 0) {
      if (elapsed_ms < min_ms) min_ms = elapsed_ms;
      if (elapsed_ms > max_ms) max_ms = elapsed_ms;
    }

    total_ms += elapsed_ms;
  }

  printf("benchmark=%s\n", cfg.name);
  printf("size=%d\n", cfg.size);
  printf("warmup=%d\n", cfg.warmup);
  printf("iterations=%d\n", cfg.iterations);
  printf("threads=%d\n",
#ifdef _OPENMP
         omp_get_max_threads()
#else
         1
#endif
  );
  printf("warmup-checksum=%lld\n", (long long)warmup_checksum);
  printf("checksum=%lld\n", (long long)state.expected);
  printf("total-ms=%.3f ms\n", total_ms);
  printf("avg-ms=%.3f ms\n", total_ms / (double)cfg.iterations);
  printf("min-ms=%.3f ms\n", min_ms);
  printf("max-ms=%.3f ms\n", max_ms);

  loom_bench_free(left);
  loom_bench_free(right);
  loom_bench_free(out);
  return 0;
}
