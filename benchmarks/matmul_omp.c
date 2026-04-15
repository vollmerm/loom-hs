#include <math.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

static double *alloc_doubles(size_t count) {
  void *ptr = NULL;
  if (posix_memalign(&ptr, 64, count * sizeof(double)) != 0) {
    fprintf(stderr, "failed to allocate %zu doubles\n", count);
    exit(1);
  }
  return (double *)ptr;
}

static void fill_seeded(double *xs, size_t count, int stride, int offset) {
  for (size_t i = 0; i < count; ++i) {
    xs[i] = (double)(((long long)i * stride + offset) % 257) / 257.0;
  }
}

static int checksum_matrix(const double *xs, int n) {
  int mid = n / 2;
  double total = xs[0] + xs[mid * n + mid] + xs[n * n - 1];
  return (int)llround(total * 1000.0);
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

static int run_once(const config_t *cfg, const double *left, const double *right,
                    double *out) {
  if (cfg->kind == BENCH_NON_TILED) {
    matmul_non_tiled(cfg->size, left, right, out);
  } else {
    matmul_tiled(cfg->size, cfg->tile, left, right, out);
  }
  return checksum_matrix(out, cfg->size);
}

int main(int argc, char **argv) {
  config_t cfg = parse_args(argc, argv);
  size_t elems = (size_t)cfg.size * (size_t)cfg.size;
  double *left = alloc_doubles(elems);
  double *right = alloc_doubles(elems);
  double *out = alloc_doubles(elems);

  fill_seeded(left, elems, 17, 11);
  fill_seeded(right, elems, 31, 7);

  int warmup_checksum = 0;
  for (int i = 0; i < cfg.warmup; ++i) {
    warmup_checksum = run_once(&cfg, left, right, out);
  }

  double total_ms = 0.0;
  double min_ms = 0.0;
  double max_ms = 0.0;
  int checksum = 0;

  for (int i = 0; i < cfg.iterations; ++i) {
    double start = omp_get_wtime();
    int current = run_once(&cfg, left, right, out);
    double end = omp_get_wtime();
    double elapsed_ms = (end - start) * 1000.0;

    if (i == 0) {
      checksum = current;
      min_ms = elapsed_ms;
      max_ms = elapsed_ms;
    } else if (current != checksum) {
      fprintf(stderr, "checksum changed across iterations: expected %d got %d\n",
              checksum, current);
      free(left);
      free(right);
      free(out);
      return 1;
    } else {
      if (elapsed_ms < min_ms) min_ms = elapsed_ms;
      if (elapsed_ms > max_ms) max_ms = elapsed_ms;
    }

    total_ms += elapsed_ms;
  }

  printf("benchmark=%s\n", cfg.name);
  printf("size=%d\n", cfg.size);
  printf("warmup=%d\n", cfg.warmup);
  printf("iterations=%d\n", cfg.iterations);
  printf("threads=%d\n", omp_get_max_threads());
  printf("warmup-checksum=%d\n", warmup_checksum);
  printf("checksum=%d\n", checksum);
  printf("total-ms=%.3f ms\n", total_ms);
  printf("avg-ms=%.3f ms\n", total_ms / (double)cfg.iterations);
  printf("min-ms=%.3f ms\n", min_ms);
  printf("max-ms=%.3f ms\n", max_ms);

  free(left);
  free(right);
  free(out);
  return 0;
}
