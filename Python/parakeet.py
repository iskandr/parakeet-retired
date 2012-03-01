from ctypes import c_void_p

from parakeet_ast import register_function, VisitedFunctions, list_to_ctypes_array
from parakeet_common import LibPar, LOG
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
    # todo: implement keyword arguments in parakeet
    assert len(kwds) == 0
    n_args = len(args)
    INPUT_ARRAY_TYPE = c_void_p * n_args
    inputs = INPUT_ARRAY_TYPE()
    for i in range(n_args):
      inputs[i] = python_value_to_parakeet(args[i])
    n_glob_args = len(self.global_vars)
    GLOB_ARRAY_TYPE = c_void_p * n_glob_args
    globals = GLOB_ARRAY_TYPE()
    for i in range(n_glob_args):
      val = self.__get_value(self.global_vars[i])
      print "ARGVAL:    ", val
      globals[i] = python_value_to_parakeet(val)
    ret = LibPar.run_function(self.parakeet_untyped_id, globals, n_glob_args, inputs, n_args)
    if ret.return_code != 0:
      raise RuntimeError("[Parakeet] Execution failed: %s" % ret.error_msg)
    else:
      print "Got %d results" % ret.results_len
      if ret.results_len > 0:
        return parakeet_value_to_python(ret.results[0])
      else:
        return

  def call_original(self, *args, **kwds):
    return self.old_function(*args, **kwds)

  def __get_value(self, var):
    var_parts = var.split('.')
    curr_val = self.old_function.func_globals[var_parts[0]]
    for i in range(1, len(var_parts)):
      #Regular expression to extract []
      curr_val = curr_val.__dict__[var_parts[i]]
    return curr_val

def PAR(func):
  return WrappedFunction(func)

from parakeet_lib import *
