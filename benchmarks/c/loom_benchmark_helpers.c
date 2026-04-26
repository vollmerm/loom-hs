#define _POSIX_C_SOURCE 200809L

#include "loom_benchmark_helpers.h"

#ifdef _OPENMP
#include <omp.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

static void loom_bench_die_allocation(size_t alignment, size_t bytes) {
  fprintf(stderr,
          "failed to allocate %zu bytes with %zu-byte alignment\n",
          bytes,
          alignment);
  exit(EXIT_FAILURE);
}

static void loom_bench_die_overflow(size_t count, size_t element_size) {
  fprintf(stderr,
          "allocation overflow for %zu elements of size %zu\n",
          count,
          element_size);
  exit(EXIT_FAILURE);
}

static void *loom_bench_xaligned_alloc(size_t alignment, size_t bytes) {
  void *ptr = NULL;
  if (alignment < sizeof(void *)) {
    alignment = sizeof(void *);
  }
  if (posix_memalign(&ptr, alignment, bytes) != 0) {
    loom_bench_die_allocation(alignment, bytes);
  }
  return ptr;
}

void *loom_bench_alloc_aligned(size_t alignment, size_t bytes) {
  return loom_bench_xaligned_alloc(alignment, bytes);
}

static void *loom_bench_alloc_array(size_t count, size_t element_size) {
  if (count != 0 && count > SIZE_MAX / element_size) {
    loom_bench_die_overflow(count, element_size);
  }
  return loom_bench_xaligned_alloc(LOOM_BENCH_ARRAY_ALIGNMENT, count * element_size);
}

int *loom_bench_alloc_ints(size_t count) {
  return (int *)loom_bench_alloc_array(count, sizeof(int));
}

int32_t *loom_bench_alloc_int32s(size_t count) {
  return (int32_t *)loom_bench_alloc_array(count, sizeof(int32_t));
}

int64_t *loom_bench_alloc_int64s(size_t count) {
  return (int64_t *)loom_bench_alloc_array(count, sizeof(int64_t));
}

double *loom_bench_alloc_doubles(size_t count) {
  return (double *)loom_bench_alloc_array(count, sizeof(double));
}

void loom_bench_free(void *ptr) {
  free(ptr);
}

static int loom_bench_seed_value(size_t index, int stride, int offset, int modulus) {
  long long raw = (long long)index * (long long)stride + (long long)offset;
  int value = (int)(raw % modulus);
  if (value < 0) {
    value += modulus;
  }
  return value;
}

void loom_bench_fill_seeded_int(int *xs, size_t count, int stride, int offset) {
  for (size_t i = 0; i < count; ++i) {
    xs[i] = loom_bench_seed_value(i, stride, offset, 257);
  }
}

void loom_bench_fill_seeded_int32(int32_t *xs, size_t count, int stride, int offset) {
  for (size_t i = 0; i < count; ++i) {
    xs[i] = (int32_t)loom_bench_seed_value(i, stride, offset, 257);
  }
}

void loom_bench_fill_seeded_int64(int64_t *xs, size_t count, int stride, int offset) {
  for (size_t i = 0; i < count; ++i) {
    xs[i] = (int64_t)loom_bench_seed_value(i, stride, offset, 257);
  }
}

void loom_bench_fill_seeded_double(double *xs, size_t count, int stride, int offset) {
  for (size_t i = 0; i < count; ++i) {
    xs[i] = (double)loom_bench_seed_value(i, stride, offset, 257) / 257.0;
  }
}

void loom_bench_fill_seeded_signed_double(double *xs, size_t count, int stride, int offset) {
  for (size_t i = 0; i < count; ++i) {
    double unit = (double)loom_bench_seed_value(i, stride, offset, 1021) / 1021.0;
    xs[i] = (2.0 * unit) - 1.0;
  }
}

void loom_bench_fill_seeded_positive_double(double *xs, size_t count, int stride, int offset) {
  for (size_t i = 0; i < count; ++i) {
    double unit = (double)loom_bench_seed_value(i, stride, offset, 1021) / 1021.0;
    xs[i] = 0.5 + unit;
  }
}

static size_t loom_bench_mid(size_t n) {
  return n / 2;
}

static size_t loom_bench_index_2d(size_t row, size_t col, size_t cols) {
  return (row * cols) + col;
}

static size_t loom_bench_index_3d(size_t i, size_t j, size_t k, size_t dim1, size_t dim2) {
  return ((i * dim1) + j) * dim2 + k;
}

static int64_t loom_bench_round_scaled(double value) {
  double scaled = value * 1000.0;
  return (int64_t)(scaled >= 0.0 ? scaled + 0.5 : scaled - 0.5);
}

int64_t loom_bench_sample_1d_int(const int *xs, size_t n) {
  if (n == 0) {
    return 0;
  }
  return (int64_t)xs[0] + (int64_t)xs[loom_bench_mid(n)] + (int64_t)xs[n - 1];
}

int64_t loom_bench_sample_1d_int32(const int32_t *xs, size_t n) {
  if (n == 0) {
    return 0;
  }
  return (int64_t)xs[0] + (int64_t)xs[loom_bench_mid(n)] + (int64_t)xs[n - 1];
}

int64_t loom_bench_sample_1d_int64(const int64_t *xs, size_t n) {
  if (n == 0) {
    return 0;
  }
  return xs[0] + xs[loom_bench_mid(n)] + xs[n - 1];
}

int64_t loom_bench_sample_1d_double(const double *xs, size_t n) {
  if (n == 0) {
    return 0;
  }
  double total = xs[0] + xs[loom_bench_mid(n)] + xs[n - 1];
  return loom_bench_round_scaled(total);
}

int64_t loom_bench_sample_2d_int(const int *xs, size_t rows, size_t cols) {
  if (rows == 0 || cols == 0) {
    return 0;
  }
  size_t mid_row = loom_bench_mid(rows);
  size_t mid_col = loom_bench_mid(cols);
  return (int64_t)xs[0] + (int64_t)xs[loom_bench_index_2d(mid_row, mid_col, cols)] +
         (int64_t)xs[loom_bench_index_2d(rows - 1, cols - 1, cols)];
}

int64_t loom_bench_sample_2d_int32(const int32_t *xs, size_t rows, size_t cols) {
  if (rows == 0 || cols == 0) {
    return 0;
  }
  size_t mid_row = loom_bench_mid(rows);
  size_t mid_col = loom_bench_mid(cols);
  return (int64_t)xs[0] + (int64_t)xs[loom_bench_index_2d(mid_row, mid_col, cols)] +
         (int64_t)xs[loom_bench_index_2d(rows - 1, cols - 1, cols)];
}

int64_t loom_bench_sample_2d_int64(const int64_t *xs, size_t rows, size_t cols) {
  if (rows == 0 || cols == 0) {
    return 0;
  }
  size_t mid_row = loom_bench_mid(rows);
  size_t mid_col = loom_bench_mid(cols);
  return xs[0] + xs[loom_bench_index_2d(mid_row, mid_col, cols)] +
         xs[loom_bench_index_2d(rows - 1, cols - 1, cols)];
}

int64_t loom_bench_sample_2d_double(const double *xs, size_t rows, size_t cols) {
  if (rows == 0 || cols == 0) {
    return 0;
  }
  size_t mid_row = loom_bench_mid(rows);
  size_t mid_col = loom_bench_mid(cols);
  double total = xs[0] + xs[loom_bench_index_2d(mid_row, mid_col, cols)] +
                 xs[loom_bench_index_2d(rows - 1, cols - 1, cols)];
  return loom_bench_round_scaled(total);
}

int64_t loom_bench_sample_3d_int(const int *xs, size_t dim0, size_t dim1, size_t dim2) {
  if (dim0 == 0 || dim1 == 0 || dim2 == 0) {
    return 0;
  }
  size_t mid0 = loom_bench_mid(dim0);
  size_t mid1 = loom_bench_mid(dim1);
  size_t mid2 = loom_bench_mid(dim2);
  return (int64_t)xs[0] + (int64_t)xs[loom_bench_index_3d(mid0, mid1, mid2, dim1, dim2)] +
         (int64_t)xs[loom_bench_index_3d(dim0 - 1, dim1 - 1, dim2 - 1, dim1, dim2)];
}

int64_t loom_bench_sample_3d_int32(const int32_t *xs, size_t dim0, size_t dim1, size_t dim2) {
  if (dim0 == 0 || dim1 == 0 || dim2 == 0) {
    return 0;
  }
  size_t mid0 = loom_bench_mid(dim0);
  size_t mid1 = loom_bench_mid(dim1);
  size_t mid2 = loom_bench_mid(dim2);
  return (int64_t)xs[0] + (int64_t)xs[loom_bench_index_3d(mid0, mid1, mid2, dim1, dim2)] +
         (int64_t)xs[loom_bench_index_3d(dim0 - 1, dim1 - 1, dim2 - 1, dim1, dim2)];
}

int64_t loom_bench_sample_3d_int64(const int64_t *xs, size_t dim0, size_t dim1, size_t dim2) {
  if (dim0 == 0 || dim1 == 0 || dim2 == 0) {
    return 0;
  }
  size_t mid0 = loom_bench_mid(dim0);
  size_t mid1 = loom_bench_mid(dim1);
  size_t mid2 = loom_bench_mid(dim2);
  return xs[0] + xs[loom_bench_index_3d(mid0, mid1, mid2, dim1, dim2)] +
         xs[loom_bench_index_3d(dim0 - 1, dim1 - 1, dim2 - 1, dim1, dim2)];
}

int64_t loom_bench_sample_3d_double(const double *xs, size_t dim0, size_t dim1, size_t dim2) {
  if (dim0 == 0 || dim1 == 0 || dim2 == 0) {
    return 0;
  }
  size_t mid0 = loom_bench_mid(dim0);
  size_t mid1 = loom_bench_mid(dim1);
  size_t mid2 = loom_bench_mid(dim2);
  double total = xs[0] + xs[loom_bench_index_3d(mid0, mid1, mid2, dim1, dim2)] +
                 xs[loom_bench_index_3d(dim0 - 1, dim1 - 1, dim2 - 1, dim1, dim2)];
  return loom_bench_round_scaled(total);
}

int64_t loom_bench_sample_double_triple(const double *xs, const double *ys, const double *zs, size_t n) {
  if (n == 0) {
    return 0;
  }
  size_t mid = loom_bench_mid(n);
  double total = xs[0] + ys[0] + zs[0] + xs[mid] + ys[mid] + zs[mid] + xs[n - 1] + ys[n - 1] + zs[n - 1];
  return loom_bench_round_scaled(total);
}

double loom_bench_now_seconds(void) {
#ifdef _OPENMP
  return omp_get_wtime();
#else
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (double)ts.tv_sec + ((double)ts.tv_nsec / 1000000000.0);
#endif
}

void loom_bench_checksum_state_init(loom_bench_checksum_state *state) {
  state->expected = 0;
  state->initialized = 0;
}

int loom_bench_checksum_state_update(loom_bench_checksum_state *state, int64_t checksum) {
  if (!state->initialized) {
    state->expected = checksum;
    state->initialized = 1;
    return 1;
  }
  return state->expected == checksum;
}
