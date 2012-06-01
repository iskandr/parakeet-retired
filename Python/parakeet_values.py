import numpy as np
import ctypes
from ctypes import *
from parakeet_common import LibPar, LOG, list_to_ctypes_array

###############################################################################
#  Type conversions 
###############################################################################

numpy_to_c_types = {
  np.bool_: c_bool,
  np.int8 : c_char, 
  np.int32: c_int32,
  np.int64: c_int64,
  np.float32: c_float,
  np.float64: c_double,
}

parakeet_to_c_types = {
  LibPar.bool_t: c_int,
  LibPar.char_t: c_char,
  LibPar.int32_t: c_int32,
  LibPar.int64_t: c_int64,
  LibPar.float32_t: c_float,
  LibPar.float64_t: c_double,
}

numpy_to_parakeet_types = {
  np.bool_: LibPar.bool_t,
  np.int8 : LibPar.char_t, 
  np.int32: LibPar.int32_t,
  np.int64: LibPar.int64_t,
  np.float32: LibPar.float32_t,
  np.float64: LibPar.float64_t,
}


parakeet_to_dtype = { 
  LibPar.bool_t: np.bool_,
  LibPar.char_t: np.int8,
  LibPar.int32_t : np.int32,
  LibPar.int64_t : np.int64,
  LibPar.float32_t: np.float32,
  LibPar.float64_t: np.float64,
}

def is_array(arg):
 return isinstance(arg, np.ndarray) and np.rank(arg) > 0 

def is_zero_rank_array(arg):
  return isinstance(arg, np.ndarray) and np.rank(arg) == 0

###############################################################################
#   Value conversion between parakeet and python
###############################################################################

# given a numpy array or a scalar, construct the equivalent parakeet value
def python_value_to_parakeet(arg):
  if is_array(arg):
    rank = len(arg.shape)
    inputShape = arg.ctypes.shape_as(c_int32)
    inputStrides = arg.ctypes.strides_as(c_int32)
    npEltType = arg.dtype.type
    if ((npEltType not in numpy_to_c_types) or
        (npEltType not in numpy_to_parakeet_types)):
      raise Exception("Numpy element type unsupported: " + str(npType))
    ctype = numpy_to_c_types[npEltType]
    parakeetType = numpy_to_parakeet_types[npEltType]
    dataPtr = arg.ctypes.data_as(POINTER(ctype))
    parakeetVal = LibPar.mk_host_array(dataPtr, parakeetType, inputShape, rank,
      inputStrides, rank, arg.nbytes)
    return c_void_p(parakeetVal)

  elif arg is None: 
    return LibPar.mk_none_val()
  elif np.isscalar(arg) or is_zero_rank_array(arg):
    # unpack zero rank arrays into scalars 
    if is_zero_rank_array(arg):
      arg = arg[()] 
    if type(arg) in [int, np.int32]:
      return LibPar.mk_int32(arg)
    elif type(arg) ==  np.int64:
      return LibPar.mk_int64(arg)
    elif type(arg)in [float, np.float64]:
      return LibPar.mk_float64(c_double(arg))
    elif type(arg) == np.float32:
      return LibPar.mk_float32(c_float(arg))
    elif type(arg)in [bool, np.bool_]:
      return LibPar.mk_bool(c_bool(arg))
    else:
      raise Exception ("Unknown type: " + str(type(arg)))
  else:
    raise Exception ("Input not supported by Parakeet: " + str(arg))

buffer_from_memory = ctypes.pythonapi.PyBuffer_FromReadWriteMemory
buffer_from_memory.restype = ctypes.py_object
buffer_from_memory.argtypes = [ctypes.c_void_p, ctypes.c_int] 

def array_from_memory(pointer, shape, strides, dtype):
  arr = np.empty(shape=shape, dtype=dtype)
  arr.strides = strides
  #print "Updated strides"
  #print "about to call buffer_from_memory"
  arr.data = buffer_from_memory(pointer, arr.nbytes)
  #print "Called!"
  #print "arr[0]", arr[0]
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

    STRIDES_TYPE = c_int * val.data.array.strides_len
    c_strides = STRIDES_TYPE.from_address(val.data.array.strides)

    parakeet_elt_type = LibPar.get_array_element_type(val.data.array.ret_type)
    dtype = parakeet_to_dtype[parakeet_elt_type]
    addr = val.data.array.data 
    shape = tuple(c_shape)
    strides = tuple(c_strides)
    ndarray = array_from_memory(addr, shape, strides, dtype)
    return ndarray
