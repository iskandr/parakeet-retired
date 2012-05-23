import ast 
import numpy as np 
import inspect 
from parakeet_common import LibPar, list_to_ctypes_array  
from parakeet_ast import ParakeetOperators, ASTConverter, global_fn_name
from parakeet_values import python_value_to_parakeet
from ctypes import *


###############################################################################
#  Function Registration`
###############################################################################

#Keeps track of the user-made functions that have been made and the built-ins
VisitedFunctions = ParakeetOperators.copy()
VisitedFunctions[np.array] = None
VisitedFunctionGlobals = {}


def ast_to_str(node, 
               annotate_fields=True, include_attributes=False, indent='  '):
    """
    Return a formatted dump of the tree in *node*.  This is mainly useful for
    debugging purposes.  The returned string will show the names and the values
    for fields.  This makes the code impossible to evaluate, so if evaluation is
    wanted *annotate_fields* must be set to False.  Attributes such as line
    numbers and column offsets are not dumped by default.  If this is wanted,
    *include_attributes* can be set to True.
    """
    def _format(node, level=0):
        if isinstance(node, ast.AST):
            fields = [(a, _format(b, level)) for a, b in ast.iter_fields(node)]
            if include_attributes and node._attributes:
                fields.extend([(a, _format(getattr(node, a), level))
                               for a in node._attributes])
            return ''.join([
                node.__class__.__name__,
                '(',
                ', '.join(('%s=%s' % field for field in fields)
                           if annotate_fields else
                           (b for a, b in fields)),
                ')'])
        elif isinstance(node, list):
            lines = ['[']
            lines.extend((indent * (level + 2) + _format(x, level + 2) + ','
                         for x in node))
            if len(lines) > 1:
                lines.append(indent * (level + 1) + ']')
            else:
                lines[-1] += ']'
            return '\n'.join(lines)
        return repr(node)
    if not isinstance(node, ast.AST):
        raise TypeError('expected AST, got %r' % node.__class__.__name__)
    return _format(node)



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

