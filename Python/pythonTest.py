import ast
from ctypes import *
import os

libcude = cdll.LoadLibrary

libcude = cdll.LoadLibrary('/usr/local/cuda/lib/libcudart.so.3')
libtest = cdll.LoadLibrary(os.getcwd() + '/../_build/libparakeetpy.so')
libtest.parakeet_init()
#libtest.ast_init()
#libtest.front_end_init()

libtest.mk_int64_paranode.restype = c_void_p
libtest.mk_var.restype = c_void_p
libtest.mk_scalar_op.restype = c_void_p
libtest.mk_app.restype = c_void_p
libtest.mk_lam.restype = c_void_p


INT = c_void_p(libtest.mk_int64_paranode(2,None))
VAR = c_void_p(libtest.mk_var(c_char_p("x"),None))
ADD = c_void_p(libtest.mk_scalar_op(0,None))
print "added"
LIST = c_void_p * 2
ARGS = LIST(VAR,INT)
print "list made"
APP = c_void_p(libtest.mk_app(ADD,ARGS,2,None))
print "apped"
VARLIST = c_char_p * 1
VARS = VARLIST(c_char_p("x"))
LAM = c_void_p(libtest.mk_lam(VARS,1,APP,None))
GLOBLIST = c_char_p * 0
GLOB = GLOBLIST()
#libtest.register_untyped_function(c_char_p("add2"),GLOB,0,VARS,1,c_void_p(libtest.mk_lam(VARS,1,c_void_p(libtest.mk_app(c_void_p(libtest.mk_scalar_op(0,None)),ARGS,2,None)),None)))
libtest.register_untyped_function(c_char_p("add2"),GLOB,0,VARS,1,LAM)
