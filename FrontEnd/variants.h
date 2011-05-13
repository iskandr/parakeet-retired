/*
 *  variants.h
 *
 *  Enumerations that represent the numerical values of the OCaml Variant types
 *  we use in the C interface.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2011.
 */

#ifndef VARIANTS_H_
#define VARIANTS_H_

/**
  * Needs to be updated if the types change!
  *
  * I put the non-data Variants first for each type, then the data versions
 **/

enum run_template_ret_val_no_data {
  Pass = 0
};
enum run_template_ret_val_data {
  Success = 0,
  Error
};

typedef enum dyn_type_with_data {
  VecT = 0,
  TupleT,
  TableT,
  FnT
} dyn_type_with_data_t;

enum pqnum_tag {
  PQNUM_BOOL = 0,
  PQNUM_CHAR,
  PQNUM_UINT16,
  PQNUM_INT16,
  PQNUM_UINT32,
  PQNUM_INT32,
  PQNUM_UINT64,
  PQNUM_INT64,
  PQNUM_FLOAT32,
  PQNUM_FLOAT64,
  PQNUM_INF,
  PQNUM_NEGINF
};

enum host_val_tag {
    HostScalar = 0,
    HostArray,
    HostBoxedArray
};

enum host_array_layout {
    HostArray_PTR = 0,
    HostArray_HOST_T,
    HostArray_SHAPE,
    HostArray_NBYTES
};

/* needs to stay synchronized with definition in GpuVal.ml */
enum gpu_val_tag { GpuScalar = 0, GpuArray };

enum gpu_array_layout {
    GpuArray_VEC_PTR = 0,
    GpuArray_VEC_NBYTES,
    GpuArray_VEC_LEN,
    GpuArray_VEC_SHAPE_PTR,
    GpuArray_VEC_SHAPE_NBYTES,
    GpuArray_VEC_SHAPE,
    GpuArray_VEC_T
};

enum pqnum_gpu_arg_tag {
  PQNUM_GPU_SCALAR_ARG = 0,
  PQNUM_GPU_ARRAY_ARG
};

enum pqnum_gpu_array_arg_layout {
  PQNUM_GPU_ARRAY_ARG_PTR = 0,
  PQNUM_GPU_ARRAY_ARG_NBYTES
};

#endif
