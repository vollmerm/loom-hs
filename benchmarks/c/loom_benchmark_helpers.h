#ifndef LOOM_BENCHMARK_HELPERS_H
#define LOOM_BENCHMARK_HELPERS_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

enum { LOOM_BENCH_ARRAY_ALIGNMENT = 64 };

typedef struct {
  int64_t expected;
  int initialized;
} loom_bench_checksum_state;

void *loom_bench_alloc_aligned(size_t alignment, size_t bytes);
int *loom_bench_alloc_ints(size_t count);
int32_t *loom_bench_alloc_int32s(size_t count);
int64_t *loom_bench_alloc_int64s(size_t count);
double *loom_bench_alloc_doubles(size_t count);
void loom_bench_free(void *ptr);

void loom_bench_fill_seeded_int(int *xs, size_t count, int stride, int offset);
void loom_bench_fill_seeded_int32(int32_t *xs, size_t count, int stride, int offset);
void loom_bench_fill_seeded_int64(int64_t *xs, size_t count, int stride, int offset);
void loom_bench_fill_seeded_double(double *xs, size_t count, int stride, int offset);
void loom_bench_fill_seeded_signed_double(double *xs, size_t count, int stride, int offset);
void loom_bench_fill_seeded_positive_double(double *xs, size_t count, int stride, int offset);

int64_t loom_bench_sample_1d_int(const int *xs, size_t n);
int64_t loom_bench_sample_1d_int32(const int32_t *xs, size_t n);
int64_t loom_bench_sample_1d_int64(const int64_t *xs, size_t n);
int64_t loom_bench_sample_1d_double(const double *xs, size_t n);

int64_t loom_bench_sample_2d_int(const int *xs, size_t rows, size_t cols);
int64_t loom_bench_sample_2d_int32(const int32_t *xs, size_t rows, size_t cols);
int64_t loom_bench_sample_2d_int64(const int64_t *xs, size_t rows, size_t cols);
int64_t loom_bench_sample_2d_double(const double *xs, size_t rows, size_t cols);

int64_t loom_bench_sample_3d_int(const int *xs, size_t dim0, size_t dim1, size_t dim2);
int64_t loom_bench_sample_3d_int32(const int32_t *xs, size_t dim0, size_t dim1, size_t dim2);
int64_t loom_bench_sample_3d_int64(const int64_t *xs, size_t dim0, size_t dim1, size_t dim2);
int64_t loom_bench_sample_3d_double(const double *xs, size_t dim0, size_t dim1, size_t dim2);

int64_t loom_bench_sample_double_triple(const double *xs, const double *ys, const double *zs, size_t n);

double loom_bench_now_seconds(void);

void loom_bench_checksum_state_init(loom_bench_checksum_state *state);
int loom_bench_checksum_state_update(loom_bench_checksum_state *state, int64_t checksum);

#ifdef __cplusplus
}
#endif

#endif
