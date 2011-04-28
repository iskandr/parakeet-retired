import ast
from ctypes import *
import os

libcude = cdll.LoadLibrary

libcude = cdll.LoadLibrary('/usr/local/cuda/lib/libcudart.so.3')
libtest = cdll.LoadLibrary(os.getcwd() + '/../_build/libparakeetpy.so')
libtest.parakeet_init()
#libtest.ast_init()
#libtest.front_end_init()

libtest.mk_int32_paranode.restype = c_void_p
libtest.mk_var.restype = c_void_p
libtest.mk_scalar_op.restype = c_void_p
libtest.mk_app.restype = c_void_p
libtest.mk_lam.restype = c_void_p
libtest.mk_block.restype = c_void_p
libtest.mk_scalar.restype = c_void_p
libtest.mk_vec.restype = c_void_p
libtest.mk_host_array.restype = c_void_p
libtest.register_untyped_function.restype = c_int


class _U(Union):
  _fields_ = [("error_msg",c_char_p),
              ("results",POINTER(c_void_p))]

class return_val_t(Structure):
  _fields_ = [("return_code",c_int),
              ("ret_type",c_void_p),
			  ("num_results",c_int),
			  ("data",_U),
			  ("shapes",POINTER(POINTER(c_int)))]

libtest.run_function.restype = return_val_t




INT = c_void_p(libtest.mk_int32_paranode(2,None))
VAR = c_void_p(libtest.mk_var(c_char_p("x"),None))
ADD = c_void_p(libtest.mk_scalar_op(0,None))
print "added"
print sizeof(ADD)
print ADD
LIST = c_void_p * 2
ARGS = LIST(VAR,INT)
print "list made"
APP = c_void_p(libtest.mk_app(ADD,ARGS,2,None))
print "apped"
BLOCKLIST = c_void_p * 1
BLOCK = c_void_p(libtest.mk_block(BLOCKLIST(APP),1,None))
VARLIST = c_char_p * 1
VARS = VARLIST(c_char_p("x"))
LAM = c_void_p(libtest.mk_lam(VARS,1,APP,None))
GLOBLIST = c_char_p * 0
GLOB = GLOBLIST()
#libtest.register_untyped_function(c_char_p("add2"),GLOB,0,VARS,1,c_void_p(libtest.mk_lam(VARS,1,c_void_p(libtest.mk_app(c_void_p(libtest.mk_scalar_op(0,None)),ARGS,2,None)),None)))
add2id = c_int(libtest.register_untyped_function(c_char_p("add2"),GLOB,0,VARS,1,BLOCK))
INPUTLIST = c_int * 10
input_data = INPUTLIST(0,1,2,3,4,5,6,7,8,9)
SHAPELIST = c_int * 1
input_shape = SHAPELIST(10)
scalar_int = c_void_p(libtest.mk_scalar(c_int()))
vec_int = c_void_p(libtest.mk_vec(scalar_int))
input = c_void_p(libtest.mk_host_array(input_data, vec_int,
				 input_shape, 1, 10*sizeof(c_int)))
INPUTLIST = c_void_p * 1
inputs = INPUTLIST(input)				 
ret = return_val_t(libtest.run_function(add2id, None, 0, inputs, 1))
print("DONE")