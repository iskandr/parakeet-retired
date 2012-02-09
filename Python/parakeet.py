from ctypes import c_void_p

from parakeet_ast import register_function, VisitedFunctions
from parakeet_common import LibPar, LOG
from parakeet_values import python_value_to_parakeet, parakeet_value_to_python


class WrappedFunction:
  def __init__(self, old_function): 
    self.old_function = old_function 
    # pretend we are the same function 
    self.__name__ = old_function.__name__
    self.__module__ = old_function.__module__
    fun_id = register_function(old_function)
    assert fun_id is not None
    VisitedFunctions[self] = fun_id
    self.parakeet_untyped_id = fun_id
  
  def __call__(self, *args, **kwds):
    # todo: implement keyword arguments in parakeet
    assert len(kwds) == 0
    n_args = len(args)
    INPUT_ARRAY_TYPE = c_void_p * n_args
    inputs = INPUT_ARRAY_TYPE()
    for i in range(n_args):
      inputs[i] = python_value_to_parakeet(args[i])
    # TODO: assume there are no globals now
    ret = LibPar.run_function(self.parakeet_untyped_id, None, 0, inputs, n_args)
    if ret.return_code != 0:
      raise RuntimeError("[Parakeet] Execution failed")
    else:
      print "Got %d results" % ret.results_len
      if ret.results_len > 0:
        return parakeet_value_to_python(ret.results[0])
      else:
        return 

  def call_original(self, *args, **kwds):
    return self.old_function(*args, **kwds)

def PAR(func):
  return WrappedFunction(func)

from parakeet_lib import *
