from ctypes import *
import atexit, os, sys

verbose = True
debug = False
def LOG(msg):
  if verbose:
    print "[Parakeet]", msg
    sys.stdout.flush()

###############################################################################
#  Return value structures in C API
###############################################################################

#Return type struct
class _ret_scalar_value(Union):
  _fields_ = [("boolean", c_int),
              ("int32", c_int),
              ("int64", c_int64),
              ("float32", c_float),
              ("float64", c_double)]

class _scalar_ret_t(Structure):
  _fields_ = [("ret_type", c_void_p),
              ("ret_scalar_value", _ret_scalar_value)]

class _array_ret_t(Structure):
  _fields_ = [("ret_type", c_void_p),
              ("data", c_void_p),
              ("shape", c_void_p),
              ("shape_len", c_int),
              ("strides", c_void_p),
              ("strides_len", c_int)]

class _ret_data(Union):
  _fields_ = [("array", _array_ret_t),
              ("scalar", _scalar_ret_t)]

class _ret_t(Structure):
  _fields_ = [("data", _ret_data),
              ("is_scalar", c_int)]

class return_val_t(Structure):
  _fields_ = [("return_code", c_int),
              ("results_len", c_int),
              ("error_msg", c_char_p),
              ("results", POINTER(_ret_t))]

###############################################################################
#  Load the C library into a global named LibPar
###############################################################################

def init_parakeet_lib():
  LibPar = cdll.LoadLibrary(os.getenv('HOME') + '/.parakeet/libparakeetpy.so')
  LibPar.parakeet_init()
  
  LibPar.run_function.restype = return_val_t
  LibPar.run_adverb.restype = return_val_t 
 
  LibPar.mk_none_val.restype = c_void_p 
  LibPar.mk_int32.restype = c_void_p
  LibPar.mk_int64.restype = c_void_p
  LibPar.mk_float32.restype = c_void_p
  LibPar.mk_float64.restype = c_void_p
  LibPar.mk_bool.restype = c_void_p

  LibPar.register_untyped_function.restype = c_int
  LibPar.mk_array.restype = c_void_p
  LibPar.mk_tuple.restype = c_void_p 
  LibPar.mk_assign.restype = c_void_p
  LibPar.mk_var.restype = c_void_p
  LibPar.mk_call.restype = c_void_p
  LibPar.mk_lambda.restype = c_void_p
  LibPar.mk_block.restype = c_void_p
  LibPar.mk_host_array.restype = c_void_p
  
  
  LibPar.mk_whileloop.restype = c_void_p  
  LibPar.get_prim.restype = c_void_p
  LibPar.mk_bool_paranode.restype = c_void_p
  LibPar.mk_int32_paranode.restype = c_void_p
  LibPar.mk_int64_paranode.restype = c_void_p
  LibPar.mk_float_paranode.restype = c_void_p
  LibPar.mk_double_paranode.restype = c_void_p
  LibPar.mk_none.restype = c_void_p
  LibPar.mk_return.restype = c_void_p
  LibPar.print_ast_node.restype = c_void_p

  # Set up Parakeet configuration interface
  LibPar.set_vectorize.restype = c_void_p
  LibPar.set_multithreading.restype = c_void_p

  # Get global values for parakeet types
  LibPar.bool_t = c_int.in_dll(LibPar, "parakeet_bool_elt_t").value
  LibPar.char_t = c_int.in_dll(LibPar, "parakeet_char_elt_t").value
  LibPar.int32_t = c_int.in_dll(LibPar, "parakeet_int32_elt_t").value
  LibPar.int64_t = c_int.in_dll(LibPar, "parakeet_int64_elt_t").value
  LibPar.float32_t = c_int.in_dll(LibPar, "parakeet_float32_elt_t").value
  LibPar.float64_t = c_int.in_dll(LibPar, "parakeet_float64_elt_t").value
  return LibPar

# Load libraries, basic initialization
LibPar = init_parakeet_lib()



def list_to_ctypes_array(input_list, t = c_void_p):
  n = len(input_list)
  ARRAY_TYPE = t * n # Description of a ctypes array
  arr = ARRAY_TYPE()
  for i, x in enumerate(input_list):
    arr[i] = x
  return arr

