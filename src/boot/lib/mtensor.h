#ifndef MIKING_TENSOR_H
#define MIKING_TENSOR_H

#include <stdint.h>

typedef enum miking_result_e {
  MIKING_RESULT_OK,
  MIKING_RESULT_ERR
} miking_result_t;

typedef enum miking_tensor_elem_type_e {
  TENSOR_INT = 0,
  TENSOR_FLOAT = 1
} miking_tensor_elem_type_t;

#define MIKING_TENSOR_ELEM_SIZE 8

miking_result_t mtensor_alloc(void**, int64_t);
void mtensor_dealloc(void*);
int64_t mtensor_get_int(void*, int64_t);
double mtensor_get_float(void*, int64_t);
void mtensor_set_int(void*, int64_t, int64_t);
void mtensor_set_float(void*, int64_t, double);
void mtensor_slice(void*, int64_t, void*);
miking_result_t mtensor_copy(void*, int64_t, void**);

#endif
