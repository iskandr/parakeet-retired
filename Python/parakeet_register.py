import inspect 
from parakeet_common import LibPar 
from parakeet_ast import ASTConverter

###############################################################################
#  Function Registration`
###############################################################################

#Keeps track of the user-made functions that have been made and the built-ins
VisitedFunctions = ParakeetOperators.copy()
VisitedFunctions[np.array] = None
VisitedFunctionGlobals = {}


def register_function(f):
  print "********************************"
  print "         Registering", f
  print "********************************"
  
  if f in VisitedFunctions:
    "...already visited"
    untyped_id = VisitedFunctions[f]
    global_vars = VisitedFunctionGlobals.get(f, [])
    return untyped_id, global_vars 
  
  file_name = f.__code__.co_filename
  line_offset = f.__code__.co_firstlineno
  global_refs = f.func_globals
  
  argspec = inspect.getargspec(f)
  assert argspec.varargs is None
  assert argspec.keywords is None
  
  body_source = inspect.getsource(f) #function_source(codeInfo)
  
  body_ast = ast.parse(body_source)
  print body_source
  print ast_to_str(body_ast)
  body_ast = ast.fix_missing_locations(body_ast)
  Converter = ASTConverter(global_refs, argspec.args, file_name, line_offset)
  parakeet_syntax = Converter.visit_module(body_ast)

  for other_fn in Converter.seen_functions:
    if not VisitedFunctions.has_key(other_fn):
      register_function(other_fn)

  global_vars = list(Converter.global_variables)
  n_globals = len(global_vars)
  globals_array = list_to_ctypes_array(global_vars,c_char_p)

  if argspec.defaults is None:
    default_values = []
    positional = argspec.args 
  else:
    default_values = argspec.defaults
    positional = argspec.args[:-len(default_values)]
  
  n_defaults = len(default_values)
  n_positional = len(positional)
  positional_args_array = list_to_ctypes_array(positional, c_char_p)
  default_args = argspec.args[n_positional:]
  default_args_array = list_to_ctypes_array(default_args, c_char_p)
  parakeet_default_values = \
    [python_value_to_parakeet(v) for v in default_values]
  default_values_array = list_to_ctypes_array(parakeet_default_values)
  

  # register every function that was seen but not registered

  fn_name_c_str = c_char_p(global_fn_name(f))
  fun_id = c_int(
    LibPar.register_untyped_function(
      fn_name_c_str, 
      globals_array, n_globals, 
      positional_args_array, n_positional,
      default_args_array, default_values_array, n_defaults, 
      parakeet_syntax))

  VisitedFunctions[f] = fun_id
  VisitedFunctionGlobals[f] = global_vars 
  return fun_id, global_vars

