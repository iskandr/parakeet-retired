from ctypes import c_void_p
import re

from parakeet_ast import register_function, VisitedFunctions, \
                         list_to_ctypes_array
from parakeet_common import LibPar, LOG
from parakeet_configuration import *
from parakeet_values import python_value_to_parakeet, parakeet_value_to_python

class WrappedFunction:
  def __init__(self, old_function):
    self.old_function = old_function
    # pretend we are the same function
    self.__name__ = old_function.__name__
    self.__module__ = old_function.__module__
    fun_id, global_vars = register_function(old_function)
    assert fun_id is not None
    VisitedFunctions[self] = fun_id
    self.parakeet_untyped_id = fun_id
    self.global_vars = global_vars

  def __call__(self, *args, **kwds):
    
    n_global_args = len(self.global_vars)
    global_args = [python_value_to_parakeet(self.__get_value(g)) 
                   for g in self.global_vars]
    global_args = list_to_ctypes_array(global_args)
    
    n_args = len(args)
    arg_values = [python_value_to_parakeet(arg) for arg in args]
    arg_values = list_to_ctypes_array(arg_values)
    
    
    kwd_names = []
    kwd_values = []
    for (k,v) in kwds.items():
      kwd_names.append(c_str(k))
      kwd_values.append(python_value_to_parakeet(v))
    n_kwds = len(kwd_names)
    kwd_names = list_to_ctypes_array(kwd_names)
    kwd_values = list_to_ctypes_array(kwd_values)
    
    ret = LibPar.run_function(
        self.parakeet_untyped_id, 
        global_args, n_global_args, 
        arg_values, n_args,
        kwd_names, kwd_values, n_kwds)
    
    if ret.return_code != 0:
      raise RuntimeError("[Parakeet] Execution failed: %s" % ret.error_msg)
    else:
      #print "Got %d results" % ret.results_len
      n = ret.results_len
      if n == 0:
        return
      elif n == 1:
        return parakeet_value_to_python(ret.results[0])
      else:
        return tuple([parakeet_value_to_python(ret.results[i]) \
               for i in xrange(n)])

  def call_original(self, *args, **kwds):
    return self.old_function(*args, **kwds)

  def __get_value(self, var):
    var_parts = var.split('.')
    try:
      curr_val = self.old_function.func_globals[var_parts[0]]
    except KeyError:
      if isinstance(__builtins__,dict):
        curr_val = __builtins__[var_parts[0]]
      else:
        curr_val = getattr(__builtins__,var_parts[0])
        print "Should %s be evaluated?" % var_parts[0]
    for i in range(1, len(var_parts)):
      curr_val = curr_val.__dict__[var_parts[i]]
    return curr_val

def PAR(func):
  return WrappedFunction(func)

from parakeet_lib import *
