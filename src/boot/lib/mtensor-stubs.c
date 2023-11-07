#include "mtensor.h"

#include <string.h>

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#define Tensor_val(v) (*(void**)Data_custom_val(v))

void tensor_dealloc(value v) {
  void *t = Tensor_val(v);
  mtensor_dealloc(t);
}

struct custom_operations miking_tensor_ops = {
  .identifier   = "miking_tensor_type",
  .finalize     = tensor_dealloc,
  .compare      = custom_compare_default,
  .hash         = custom_hash_default,
  .serialize    = custom_serialize_default,
  .deserialize  = custom_deserialize_default,
  .compare_ext  = custom_compare_ext_default,
  .fixed_length = custom_fixed_length_default
};

CAMLprim value tensor_init(value size) {
  CAMLparam1(size);
  CAMLlocal1(out);
  out = caml_alloc_custom(&miking_tensor_ops, sizeof(void*), 0, 1);
  int64_t sz = Long_val(size);
  void *ptr;
  miking_result_t result = mtensor_alloc(&ptr, sz);
  if (result != MIKING_RESULT_OK) caml_failwith("Tensor construction failed");
  Tensor_val(out) = ptr;
  CAMLreturn(out);
}

CAMLprim value tensor_get(value tensor_val, value lidx, value elem_ty) {
  CAMLparam3(tensor_val, lidx, elem_ty);
  CAMLlocal1(v);
  void *t = Tensor_val(tensor_val);
  int64_t idx = Long_val(lidx);
  int64_t et = Long_val(elem_ty);
  if (et == TENSOR_INT) {
    v = Val_long(mtensor_get_int(t, idx));
  } else if (et == TENSOR_FLOAT) {
    v = caml_copy_double(mtensor_get_float(t, idx));
  } else {
    caml_failwith("Tensor get called with tensor of invalid element type");
  }
  CAMLreturn(v);
}

CAMLprim void tensor_set(value tensor_val, value lidx, value v, value elem_ty) {
  CAMLparam4(tensor_val, lidx, v, elem_ty);
  void *t = Tensor_val(tensor_val);
  int64_t idx = Long_val(lidx);
  int64_t et = Long_val(elem_ty);
  if (et == TENSOR_INT) {
    int64_t val = Long_val(v);
    mtensor_set_int(t, idx, val);
  } else if (et == TENSOR_FLOAT) {
    double val = Double_val(v);
    mtensor_set_float(t, idx, val);
  } else {
    caml_failwith("Tensor set called with tensor of invalid element type");
  }
  CAMLreturn0;
}

CAMLprim value tensor_copy(value src, value sz) {
  CAMLparam2(src, sz);
  CAMLlocal1(dst);
  int64_t size = Long_val(sz);
  void *ptr;
  miking_result_t result = mtensor_copy(Tensor_val(src), size, &ptr);
  if (result != MIKING_RESULT_OK) caml_failwith("Tensor copying failed");
  dst = caml_alloc_custom(&miking_tensor_ops, sizeof(void*), 0, 1);
  Tensor_val(dst) = ptr;
  CAMLreturn(dst);
}
