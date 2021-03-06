from ctypes import c_void_p, c_int
import re

from parakeet_common import list_to_ctypes_array, LibPar, LOG 
from parakeet_register import VisitedFunctions, VisitedFunctionGlobals
from parakeet_values import python_value_to_parakeet, parakeet_value_to_python

def _prep_value_list(vals):
  parakeet_values = [python_value_to_parakeet(v) for v in vals]
  return list_to_ctypes_array(parakeet_values)

def _prep_int_list(ints): 
  n = len(ints)
  array_t = c_int * n
  arr = array_t()
  for i, x in enumerate(ints):
    arr[i] = x 
  return arr 
  
def _prep_args(args, kwds):
  arg_values = _prep_value_list(args)
    
  kwd_names = []
  kwd_values = []
  for (k,v) in kwds.items():
    kwd_names.append(c_str(k))
    kwd_values.append(python_value_to_parakeet(v))
  kwd_names = list_to_ctypes_array(kwd_names)
  kwd_values = list_to_ctypes_array(kwd_values)
  return  arg_values, kwd_names, kwd_values 

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
        #print "[Parakeet] Should %s be evaluated?" % var_parts[0]
    for i in range(1, len(var_parts)):
      curr_val = curr_val.__dict__[var_parts[i]]
    return curr_val

  def _globals_as_python_values(self):
    return [self._get_global_value(g) for g in self.global_vars]
  
  def _globals_as_parakeet_value_list(self):
    return [python_value_to_parakeet(v) for v in self._globals_as_python_values()]
    
  def _globals_as_parakeet_value_array(self):
    return list_to_ctypes_array(self._globals_as_parakeet_value_list())
  
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

  def _run_adverb(self, adverb_name, args, kwds):

    fn_id = self.parakeet_untyped_id
    global_values = self._globals_as_python_values()
    
      
    reserved_keywords = ['axis', 'axes', 'fixed', 'combine_fixed']
    
    if 'axis' in kwds:
      axes = kwds['axis']
    elif 'axes' in kwds: 
      axes = kwds['axes']
    else:
      axes = None
      
    if axes is not None:
      try:
        iter(axes)
      except:
        axes = [axes]
        
    fixed = kwds.get('fixed', [])
    
    try:
      iter(fixed)
    except:
      fixed = [fixed]
    #TODO: make keywords work for fixed args
    fixed_values, _, _ = _prep_args(global_values + fixed, {})
    

    if adverb_name.lower() ==  "map":
      combine_provided = False
      combine_fixed_values = _prep_args([], {})
    else:
      combine_provided = True 
      if combine is None:
        wrapped_combine = self 
      else:
        wrapped_combine = PAR(combine)
      combine_id = wrapped_combine.parakeet_untyped_id
      combine_globals = wrapped_combine._globals_as_python_values()()  
    
      combine_fixed = kwds.get('combine_fixed', [])
      try:
        iter(combine_fixed)
      except:
        combine_fixed = [combine_fixed]    
      combine_fixed_values, _, _ = \
        _prep_args(combine_globals + combine_fixed, {})
    
    init = kwds.get('init', [])
    try:
      iter(init)
    except:
      init = [init]
    
    if axes is not None:
      axes_given = True
      axes_values = _prep_int_list(axes) 
      n_axes = len(axes_values)
    else:
      axes_given = False
      axes_values = None
      n_axes = 0

    init_values = _prep_value_list(init)


    filtered_kwds = \
      dict([(k,v) for (k,v) in kwds.items() if k not in reserved_keywords]) 
    array_values, array_keywords, array_keyword_values = \
      _prep_args(args, filtered_kwds)  
          
    from ctypes import c_int
    result = LibPar.run_adverb(
      adverb_name, 
      fn_id,
      fixed_values, c_int(len(fixed_values)), 
      combine_id, combine_provided,   
      combine_fixed_values, c_int(len(combine_fixed_values)), 
      init_values, c_int(len(init_values)), 
      axes_given, axes_values, c_int(n_axes), 
      array_values, c_int(len(array_values)), 
      array_keywords, array_keyword_values, c_int(len(array_keywords))) 
    return self._convert_returned_value(result)
     

  def map(self, *args, **kwds):
    return self._run_adverb("map", None, args, kwds)
    
  def reduce(self, combine = None, *args, **kwds):
    return self._run_adverb("reduce", combine, args, kwds)
  
  def scan(self, combine = None,  *args, **kwds):
    return self._run_adverb("scan", combine, args, kwds)
  
  def allpairs(self, combine = None, *args, **kwds):  
    return self._run_adverb("allpairs", combine, args, kwds)
  
  def __call__(self, *args, **kwds):
    global_values = self._prep_globals()
    arg_values, kwd_names, kwd_values = _prep_args(args, kwds)   

    ret = LibPar.run_function(
        self.parakeet_untyped_id, 
        global_values, c_int(len(global_values)), 
        arg_values, c_int(len(arg_values)),
        kwd_names, kwd_values, c_int(len(kwd_names)))
    return self._convert_returned_value(ret)
    
  def call_original(self, *args, **kwds):
    return self.old_function(*args, **kwds)



from parakeet_register import register_function 

def PAR(old_function):
  untyped_id, global_vars = register_function(old_function)
  return WrappedFunction(old_function, untyped_id, global_vars)
