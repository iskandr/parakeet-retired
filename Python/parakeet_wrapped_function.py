from ctypes import c_void_p
import re

from parakeet_common import list_to_ctypes_array, LibPar, LOG 
from parakeet_register import VisitedFunctions, VisitedFunctionGlobals
from parakeet_values import python_value_to_parakeet, parakeet_value_to_python

class WrappedFunction:
  def __init__(self, old_function, untyped_id, global_vars):
    assert untyped_id is not None
    self.old_function = old_function
    # pretend we are the same function
    self.__name__ = old_function.__name__
    self.__module__ = old_function.__module__
    VisitedFunctions[self] = untyped_id
    VisitedFunctionGlobals[self] = global_vars 
    self.parakeet_untyped_id = untyped_id
    self.global_vars = global_vars
    

  def _get_global_value(self, var):
    var_parts = var.split('.')
    try:
      curr_val = self.old_function.func_globals[var_parts[0]]
    except KeyError:
      if isinstance(__builtins__,dict):
        curr_val = __builtins__[var_parts[0]]
      else:
        curr_val = getattr(__builtins__, var_parts[0])
        print "[Parakeet] Should %s be evaluated?" % var_parts[0]
    for i in range(1, len(var_parts)):
      curr_val = curr_val.__dict__[var_parts[i]]
    return curr_val


  def _prep_args(self, args, kwds):
    global_args = [python_value_to_parakeet(self._get_global_value(g)) 
                   for g in self.global_vars]
    global_args = list_to_ctypes_array(global_args)
    
    arg_values = [python_value_to_parakeet(arg) for arg in args]
    arg_values = list_to_ctypes_array(arg_values)
    
    kwd_names = []
    kwd_values = []
    for (k,v) in kwds.items():
      kwd_names.append(c_str(k))
      kwd_values.append(python_value_to_parakeet(v))
    kwd_names = list_to_ctypes_array(kwd_names)
    kwd_values = list_to_ctypes_array(kwd_values)
    return global_args, arg_values, kwd_names, kwd_values 

  def _convert_returned_value(self, ret):    
    if ret.return_code != 0:
      raise RuntimeError("[Parakeet] Execution failed: %s" % ret.error_msg)
    else:
      n = ret.results_len 
      if n == 0:
        return
      elif n == 1:
        return parakeet_value_to_python(ret.results[0])
      else:
        results = [parakeet_value_to_python(ret.results[i]) for i in xrange(n)]
        return tuple(results)

  def map(self, *args, **kwds):
    globals, arg_values, kwd_names, kwd_values = self._prep_args(args, kwds)    
    ret = LibPar.map(
        self.parakeet_untyped_id, 
        globals, len(globals), 
        arg_values, len(arg_values),
        kwd_names, kwd_values, len(kwd_names))
    return self._convert_returned_value(ret)
    
  def reduce(self, *args, **kwds):
    globals, arg_values, kwd_names, kwd_values = self._prep_args(args, kwds)
    ret = LibPar.reduce(
        self.parakeet_untyped_id, 
        globals, len(globals), 
        arg_values, len(arg_values),
        kwd_names, kwd_values, len(kwd_names))
    return self._convert_returned_value(ret)
    
  def scan(self, *args, **kwds):
    globals, arg_values, kwd_names, kwd_values = self._prep_args(args, kwds)
    ret = LibPar.scan(
        self.parakeet_untyped_id, 
        globals, len(globals), 
        arg_values, len(arg_values),
        kwd_names, kwd_values, len(kwd_names))
    return self._convert_returned_value(ret)
  
  def allpairs(self, *args, **kwds):  
    globals, arg_values, kwd_names, kwd_values = self._prep_args(args, kwds)
    ret = LibPar.allpairs(
        self.parakeet_untyped_id, 
        globals, len(globals), 
        arg_values, len(arg_values),
        kwd_names, kwd_values, len(kwd_names))
    return self._convert_returned_value(ret)
                                                                     
  def __call__(self, *args, **kwds):
    globals, arg_values, kwd_names, kwd_values = self._prep_args(args, kwds)    
    ret = LibPar.run_function(
        self.parakeet_untyped_id, 
        globals, len(globals), 
        arg_values, len(arg_values),
        kwd_names, kwd_values, len(kwd_names))
    return self._convert_returned_value(ret)
    
  def call_original(self, *args, **kwds):
    return self.old_function(*args, **kwds)


from parakeet_register import register_function 

def PAR(old_function):
  untyped_id, global_vars = register_function(old_function)
  return WrappedFunction(old_function, untyped_id, global_vars)


