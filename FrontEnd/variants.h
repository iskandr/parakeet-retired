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

enum parnum_tag {
  PARNUM_BOOL = 0,
  PARNUM_CHAR,
  PARNUM_INT16,
  PARNUM_INT32,
  PARNUM_INT64,
  PARNUM_FLOAT32,
  PARNUM_FLOAT64,
};

/*
enum run_template_ret_val_no_data {
  Pass = 0
};
enum run_template_ret_val_data {
  Success = 0,
  Error
};
*/ 
/*
typedef enum dyn_type_no_data {
  BottomT = 0,
  AnyT,
  UnitT,
  BoolT,
  CharT,
  Int16T,
  Int32T,
  Int64T,
  Float32T,
  Float64T,
  SymT,
  StrT,
  BottomFnT,
  AnyFnT
} dyn_type_no_data_t;
*/ 
/*
typedef enum dyn_type_with_data {
  VecT = 0,
  TupleT,
  TableT,
  FnT
} dyn_type_with_data_t;
*/ 

/*
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
*/ 
/* needs to stay synchronized with definition in GpuVal.ml */
/*enum gpu_val_tag { GpuScalar = 0, GpuArray };

enum gpu_array_layout {
    GpuArray_VEC_PTR = 0,
    GpuArray_VEC_NBYTES,
    GpuArray_VEC_LEN,
    GpuArray_VEC_SHAPE_PTR,
    GpuArray_VEC_SHAPE_NBYTES,
    GpuArray_VEC_SHAPE,
    GpuArray_VEC_T
};

enum parnum_gpu_arg_tag {
  PARNUM_GPU_SCALAR_ARG = 0,
  PARNUM_GPU_ARRAY_ARG
};

enum parnum_gpu_array_arg_layout {
  PARNUM_GPU_ARRAY_ARG_PTR = 0,
  PARNUM_GPU_ARRAY_ARG_NBYTES
};
*/ 
#endif
