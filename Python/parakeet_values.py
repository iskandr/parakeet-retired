import numpy as np
from ctypes import *
from parakeet_common import LibPar, LOG, list_to_ctypes_array

###############################################################################
#  Type conversions 
###############################################################################

numpy_to_c_types = {
  np.int32: c_int32,
  np.int64: c_int64,
  np.float32: c_float,
  np.float64: c_double,
  np.bool_: c_bool,
}

numpy_to_parakeet_types = {
  np.int32: LibPar.int32_t,
  np.int64: LibPar.int64_t,
  np.float32: LibPar.float32_t,
  np.float64: LibPar.float64_t,
  np.bool_: LibPar.bool_t
}

parakeet_to_c_types = {
  LibPar.int32_t: c_int32,
  LibPar.int64_t: c_int64,
  LibPar.float32_t: c_float,
  LibPar.float64_t: c_double,
  LibPar.bool_t: c_int,
  LibPar.char_t: c_char
}

#Builds a ctypes list of the type out of the input_list


###############################################################################
#   Value conversion between parakeet and python
###############################################################################



# given a numpy array or a scalar, construct the equivalent parakeet value
def python_value_to_parakeet(arg):
  if isinstance(arg, np.ndarray):
    rank = len(arg.shape)
    inputShape = arg.ctypes.shape_as(c_int32)
    inputStrides = arg.ctypes.strides_as(c_int32)
    if rank > 1 and not arg.flags['C_CONTIGUOUS']:
      # until we have a proper interface for telling parakeet this data is
      # column-major, we have to manually transpose it
      # TODO: wouldn't strides be enough to handle this?
      arg = np.transpose(arg).copy()
    npType = arg.dtype.type
    if ((npType not in numpy_to_c_types) or
        (npType not in numpy_to_parakeet_types)):
      raise Exception("Numpy element type unsupported: " + str(npType))
    ctype = numpy_to_c_types[npType]
    parakeetType = numpy_to_parakeet_types[npType]
    dataPtr = arg.ctypes.data_as(POINTER(ctype))
    parakeetVal = LibPar.mk_host_array(dataPtr, parakeetType, inputShape, rank,
                                       inputStrides, rank, arg.nbytes)
    return c_void_p(parakeetVal)
  elif np.isscalar(arg):
    if type(arg) == int:
      return LibPar.mk_int32(arg)
    elif type(arg) == float or type(arg) == np.float64:
      return LibPar.mk_float64(c_double(arg))
    elif type(arg) == np.float32:
      return LibPar.mk_float32(c_float(arg))
    elif type(arg) == bool:
      return LibPar.mk_bool(c_bool(arg))
    else:
      raise Exception ("Unknown type: " + str(type(arg)))
  else:
    raise Exception ("Input not supported by Parakeet: " + str(arg))

def array_from_memory(pointer, shape, dtype):
  from_memory = ctypes.pythonapi.PyBuffer_FromReadWriteMemory
  from_memory.restype = ctypes.py_object
  arr = np.empty(shape=shape,dtype=dtype)
  arr.data = from_memory(pointer,arr.nbytes)
  return arr

def parakeet_value_to_python(val):
  if val.is_scalar:
    c_type = parakeet_to_c_types[val.data.scalar.ret_type]
    result = 0
    if c_type == c_bool:
      result = val.data.scalar.ret_scalar_value.boolean
    elif c_type == c_int:
      result = val.data.scalar.ret_scalar_value.int32
    elif c_type == c_int64:
      result = val.data.scalar.ret_scalar_value.int64
    elif c_type == c_float:
      result = val.data.scalar.ret_scalar_value.float32
    elif c_type == c_double:
      result = val.data.scalar.ret_scalar_value.float64
    else:
      raise RuntimeError("Return type not supported by Parakeet: " %
                         str(c_type))
    return result
  else:    
    rank = val.data.array.shape_len

    SHAPE_TYPE = c_int * rank
    c_shape = SHAPE_TYPE.from_address(val.data.array.shape)
    shape = list(c_shape)

    STRIDES_TYPE = c_int * val.data.array.strides_len
    c_strides = STRIDES_TYPE.from_address(val.data.array.strides)
    strides = list(c_strides)

    parakeet_elt_type = LibPar.get_array_element_type(val.data.array.ret_type)
    c_elt_type = ParakeetTypeToCtype[parakeet_elt_type]
    nelts = reduce(lambda x,y: x * y, shape)
    ARRAY_TYPE = c_elt_type * nelts
    result_array = ARRAY_TYPE.from_address(val.data.array.data)

    np_result = np.ctypeslib.as_array(result_array)
    np_result.shape = shape
    np_result.strides = strides
    return np_result

