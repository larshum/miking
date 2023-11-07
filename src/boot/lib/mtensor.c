#include "mtensor.h"

#include <stdlib.h>
#include <string.h>

#include <stdio.h>

#ifdef MIKING_CUDA
#include <cuda_runtime.h>
#endif

miking_result_t mtensor_alloc(void **t, int64_t sz) {
  if (sz == 0) {
    *t = NULL;
    return MIKING_RESULT_OK;
  }
  sz = sz * MIKING_TENSOR_ELEM_SIZE;
#ifdef MIKING_CUDA
  if (cudaMallocManaged(t, sz, cudaMemAttachGlobal) != cudaSuccess) {
    return MIKING_RESULT_ERR;
  }
#else
  *t = malloc(sz);
#endif
  if (*t) return MIKING_RESULT_OK;
  else return MIKING_RESULT_ERR;
}

void mtensor_dealloc(void *t) {
#ifdef MIKING_CUDA
  cudaFree(t);
#else
  free(t);
#endif
}

int64_t mtensor_get_int(void *t, int64_t idx) {
  return ((int64_t*)t)[idx];
}

double mtensor_get_float(void *t, int64_t idx) {
  return ((double*)t)[idx];
}

void mtensor_set_int(void *t, int64_t idx, int64_t v) {
  ((int64_t*)t)[idx] = v;
}

void mtensor_set_float(void *t, int64_t idx, double v) {
  ((double*)t)[idx] = v;
}

void mtensor_slice(void *src, int64_t idx, void *dst) {
  dst = (char*)src + idx * MIKING_TENSOR_ELEM_SIZE;
}

miking_result_t mtensor_copy(void *src, int64_t size, void **dst) {
  miking_result_t result = mtensor_alloc(dst, size);
  if (result != MIKING_RESULT_OK) return result;
  memcpy(*dst, src, size * MIKING_TENSOR_ELEM_SIZE);
  return MIKING_RESULT_OK;
}
