import ast, math, numpy as np, parakeet_lib
from ctypes import *
import parakeet_common
from parakeet_common import LibPar, LOG, list_to_ctypes_array

###############################################################################
#  C types struct for source information
###############################################################################

class _c_source_info_t(Structure):
  _fields_ = [("filename", c_char_p),
              ("line", c_int),
              ("col", c_int)]

class _source_info_t:
  def __init__(self, c_src):
    self.c_source_info = c_src
    if c_src is not None:
      self.addr = addressof(c_src)
    else:
      self.addr = 0

###############################################################################
#  Global variables
###############################################################################

ParakeetOperators = {
  np.add:'add',
  abs: 'abs',
  np.arange: 'range',
  math.exp:'exp',
  math.log:'log',
  math.pow:'pow',
  math.sqrt:'sqrt',
  parakeet_lib.map:'map',
  parakeet_lib.reduce:'reduce',
  parakeet_lib.scan:'scan',
  parakeet_lib.allpairs:'allpairs',
  np.size:'size',
}

AutoTranslate = {
#  map: parakeet_lib.map,
#  reduce: parakeet_lib.reduce,
  np.sum:parakeet_lib.sum,
  sum:parakeet_lib.sum,
  np.argmin:parakeet_lib.argmin,
  np.mean:parakeet_lib.mean,
  np.all:parakeet_lib.all,
  len:parakeet_lib._len,
  abs:parakeet_lib.abs,
}

BuiltinPrimitives = {
  'Add':'add',
  'Sub':'sub',
  'Mult':'mult',
  'Div':'div',
  'Mod':'mod',
  'Pow':'pow',
  'LShift':None,
  'RShift':None,
  'BitOr':None,
  'BitXor':None,
  'BitAnd':None,
  'FloorDiv':None,
  'Invert':None,
  'Not':'not',
  'And':'and',
  'Or':'or',
  'UAdd':None,
  'USub':'neg',
  'Eq':'eq',
  'NotEq':'neq',
  'Lt':'lt',
  'LtE':'lte',
  'Gt':'gt',
  'GtE': 'gte',
  'Is': None,
  'IsNot': None,
  'In': None,
  'NotIn': None,
  'Index': 'index',
  'Slice': 'slice',
}

BuiltinStrs = [
  "False",
  "True"]

NumpyArrayMethods = {
  "transpose": "transpose",
  "flatten": "flatten",
  "copy": "copy",
}
NumpyArrayAttributes = {
  "shape": "shape",
  "strides": "strides",
}

ValidObjects = {
  np.add: parakeet_lib.add,
  np.subtract: parakeet_lib.sub,
  np.multiply: parakeet_lib.mult,
  np.divide: parakeet_lib.div
}

Adverbs = [parakeet_lib.map, parakeet_lib.reduce,
           parakeet_lib.allpairs, parakeet_lib.scan]



#Keeps track of the user-made functions that have been made and the built-ins
VisitedFunctions = ParakeetOperators.copy()
VisitedFunctions[np.array] = ''

###############################################################################
#  Helper functions
###############################################################################

def src_addr(src_info):
  if src_info is None: 
    return None
  else: 
    return src_info.addr


def build_var(name, src_info = None):
  #Special case for booleans
  if name == 'True':
    return LibPar.mk_bool_paranode(1, src_addr(src_info))
  elif name == 'False':
    return LibPar.mk_bool_paranode(0, src_addr(src_info))
  else:
    return LibPar.mk_var(c_char_p(name), src_addr(src_info))



class ParakeetUnsupported(Exception):
  def __init__(self, value):
    self.value = value
  def __str__(self):
    return "[Parakeet] " + repr(self.value)


# always assume functions have a module but
def global_fn_name(fn, default_name="<unknown_function>"):
  if hasattr(fn, '__module__'):
    moduleName = fn.__module__
  else:
    moduleName = fn.__class__.__module__
  if hasattr(fn, '__name__'):
    return moduleName + "." + fn.__name__
  else:
    return moduleName + "." + default_name

def build_fn_node(python_fn, src_info):
  if python_fn in ParakeetOperators:
    parakeet_prim_name = ParakeetOperators[python_fn]
    parakeet_fn = ast_prim(parakeet_prim_name)
    if parakeet_prim_name is None:
      raise ParakeetUnsupported("Support for %s not implemented" %
                         python_fn)
  elif python_fn in NumpyArrayMethods:
    parakeet_prim_name = NumpyArrayMethods[python_fn]
    parakeet_fn = ast_prim(parakeet_prim_name)
    if parakeet_prim_name is None:
      raise ParakeetUnsupported("[Parakeet] Support for %s not implemented" %
                        python_fn)
  elif python_fn in NumpyArrayAttributes:
    parakeet_prim_name = NumpyArrayAttributes[python_fn]
    parakeet_fn = ast_prim(parakeet_prim_name)
    if parakeet_prim_name is None:
      raise ParakeetUnsupported("[Parakeet] Support for %s not implemented" %
                        python_fn)
  else:
    parakeet_fn = build_var(global_fn_name(python_fn), src_info)
  return parakeet_fn



def mk_call(fn, positional_args, kw_names = [], kw_values = [], src_info=None):
  """
    lower-level helper for building a function call, 
    Assumes function is a Parakeet node and that keyword
    names have already been converted to character pointers
  """
  assert len(kw_names) == len(kw_values)
  
  args_array = list_to_ctypes_array(positional_args)
  n_args = len(positional_args)
  kw_names_array = list_to_ctypes_array(kw_names)
  kw_values_array = list_to_ctypes_array(kw_values)
  n_kwds = len(kw_names)
  srcAddr = src_addr(src_info)
  
  print "mk_call", kw_names_array, kw_values_array   
  # paranode fun, paranode *args, int num_args,
  # char** keywords, paranode* keyword_values, int num_keyword_args,
  #  source_info_t *src_info
  return LibPar.mk_call(
    fn, args_array, n_args, kw_names_array, kw_values_array, n_kwds, srcAddr)

def build_call(parakeet_fn, args, kw_args = {}, src_info = None):
  """
    higher-level helper for building a function call. 
    Assumes that the function is a Python object which needs
    to be translated into a Parakeet node 
  """
  
  kw_names = []
  kw_values = []
  for k,v in kw_args.items():
    kw_names.append(c_char_p(k))
    kw_values.append(v)
  print "build_call", parakeet_fn, args, kw_names, kw_values 
  return mk_call(parakeet_fn, args, kw_names, kw_values, src_info)
   
#Function to get ast node(s) for built-in functions/primitives
def ast_prim(sym):
  return c_void_p(LibPar.get_prim(sym))

def build_prim_call(python_op_name, args, src_info = None):
  """
    Given the Python AST name for a primitive operator, 
    translate it to the equivalent Parakeet primitive and
    create a Call node for that primitive
  """
  parakeet_op_name = BuiltinPrimitives[python_op_name]
  if parakeet_op_name is None:
    raise ParakeetUnsupported('Prim not implemented: %s' % python_op_name)
  
  prim = ast_prim(parakeet_op_name)
  res =  mk_call(prim, args, src_info = src_info)
  return res 

def name_of_ast_node(op):
  return op.__class__.__name__

def flatten_var_attrs(node):
  """
  Given an AST subtree of attributes on top of a Name, 
  flatten all the variables into a single list
  """
  parts = []
  while not isinstance(node, ast.Name):
    assert isinstance(node, ast.Name) or isinstance(node, ast.Attribute)
    parts.append(node.attr) 
    node = node.value
  parts.append(node.id)  
  return list(reversed(parts))

def get_global_var_name(node):
  return ".".join(flatten_var_attrs(node))
  
def mk_return(elts, src_info=None):
  arr = list_to_ctypes_array(elts) if len(elts) > 0 else None
  return LibPar.mk_return(arr, len(elts), src_addr(src_info))

def mk_array(elts, src_info = None):
  arr = list_to_ctypes_array(elts)
  return LibPar.mk_array(arr, len(elts), src_addr(src_info))

def mk_block(stmts, src_info = None):
  arr = list_to_ctypes_array(stmts)
  return LibPar.mk_block(arr, len(stmts), src_addr(src_info))



def build_num(num, src_info = None):
  """
  Given the string form of a number, build a syntax node for an int or float
  """
  num_type = type(num)
  if num_type == int:
    return LibPar.mk_int32_paranode(num, src_addr(src_info))
  elif num_type == long: 
    return LibPar.mk_int64_paranode(num, src_addr(src_info))
  elif num_type == float:
    return LibPar.mk_double_paranode(c_double(num), src_addr(src_info))

  else:
    raise ParakeetUnsupported("Unsupported numeric type " + num_type)




###############################################################################
#  Converter
###############################################################################

class InvalidFunction(Exception):
  def __init__(self, value):
    self.value = value
  def __str__(self):
    return repr(self.value)


class ASTConverter():
  def __init__(self, global_refs, arg_names, file_name, line_offset):
    self.seen_functions = set([])
    self.arg_names = arg_names
    self.global_variables = set()
    self.file_name = file_name
    self.line_offset = line_offset
    self.global_refs = global_refs


  def visit_module(self, node):
    """
     when we parse Python source, we get a single function definition 
     wrapped by a Module node
    """ 
    assert isinstance(node, ast.Module)
    assert len(node.body) == 1
    return self.visit_function_def(node.body[0])
    #print "leaving visit_module"
  
  # TODO: don't ignore function decorators!  
  def visit_function_def(self, node):
    print "visit_function_def"
    assert isinstance(node, ast.FunctionDef)
    # we don't support variable argument functions
    assert node.args.vararg is None
    # we don't support dictionaries of keyword arguments  
    assert node.args.kwarg is None
    # assume the function arguments have been pulled out elsewhere and 
    # we just need the body 

    return self.visit_stmt_sequence(node.body)
    
  
  def visit_stmt_sequence(self, stmts, src_info=None):
    print "visit_stmt_seq"
    return mk_block([self.visit_stmt(stmt) for stmt in stmts], src_info)

  def visit_stmt(self, node, src_info = None):
    print "visit_stmt", node
    src_info = self.build_src_info(node)
    srcAddr = src_addr(src_info)
    if isinstance(node, ast.If):
      test = self.visit_expr(node.test)
      if_true = self.visit_stmt_sequence(node.body, src_info)
      if_false = self.visit_stmt_sequence(node.orelse, src_info)
      return LibPar.mk_if(test, if_true, if_false, srcAddr)
    elif isinstance(node, ast.Assign):
      assert len(node.targets) == 1
      return self.visit_assign(node.targets[0], node.value, src_info)
    elif isinstance(node, ast.Return):
      return self.visit_return(node, src_info)
      
    elif isinstance(node, ast.While):
      # infrequently used final iteration, not supported for now
      assert node.orelse is None
      block = self.visit_stmt_sequence(node.body)
      test = self.visit_exp(node.test)
      return LibPar.mk_whileloop(test, block, srcAddr)
    elif isinstance(node, ast.Expr):
      return self.visit_expr(node.value)
    else:
      raise RuntimeError("Unsupported statement" + str(node))

  def visit_assign(self, lhs, rhs, src_info = None):
    """
    On the left-hand-side of an assignment we allow either a variable
    or a tuple of variables. Anything else should signal an error.
    The right-hand-side is expected to be an expression (and not a statement).
    """  
    def mk_lhs_var(node):
      return build_var(node.id, self.build_src_info(node))
    if isinstance(lhs, ast.Name):
      vars = [mk_lhs_var(lhs)]
    elif isinstance(lhs, ast.Tuple):
      assert all([isinstance(elt, ast.Name) for elt in lhs.elts])
      vars = [mk_lhs_var(elt) for elt in lhs.elts]
    else:
      raise RuntimeError("Unsupported LHS")
    rhs = self.visit_expr(rhs)
    return LibPar.mk_assign(list_to_ctypes_array(vars), len(vars), rhs)
       
  def visit_return(self, node, src_info = None):
    if node.value is None:
      values = []
    elif isinstance(node.value, ast.Tuple): 
      values = [self.visit_expr(v) for v in node.value.elts]
    else:
      values = [self.visit_expr(node.value)]
    print values 
    return mk_return(values, src_info)


  def get_function_ref(self, node):
    if isinstance(node, ast.Name):
      funName = node.id
      if funName in self.global_refs:
        funRef = self.global_refs[funName]
      elif funName in __builtins__:
        funRef = __builtins__[funName]
      else:
        raise InvalidFunction(funName)
    elif isinstance(node, ast.Attribute):
      #For function calls like mod1.mod2.mod4.fun()
      moduleList = []
      nextNode = node
      funName = node.attr
      #Get a list of the chain of modules
      while isinstance(nextNode, ast.Attribute):
        moduleList = [nextNode.attr] + moduleList
        nextNode = nextNode.value
      currModule = self.global_refs[nextNode.id]

      for m in moduleList:
        try:
          currModule = currModule.__dict__[m]
        except AttributeError:
          if currModule in ValidObjects:
            currModule = currModule.__getattribute__(m)
          elif m in NumpyArrayMethods and m == moduleList[-1]:
            currModule = currModule.__getattribute__(m)
          elif m in NumpyArrayMethods:
            raise InvalidFunction(moduleList.join('.'))
          else:
            raise InvalidFunction("Invalid object %s" % currModule)
      funRef = currModule
    else:
      raise InvalidFunction("Call.func shouldn't be " + name)
    
    if not hasattr(funRef, '__call__'):
      return None
    if funRef in AutoTranslate:
      funRef = AutoTranslate[funRef]
    if not hasattr(funRef,'__self__') or not funRef.__self__:
      self.seen_functions.add(funRef)
    return funRef

  def register_if_function(self, node):
    #Alex: Right now this will crash if a local variable method is an argument,
    #because the local method won't be registered or translated
    try:
      parts = flatten_var_attrs(node)
      if parts[0] in self.arg_names and len(parts) > 1:
        raise ParakeetUnsupported(\
          "[Parakeet] %s is not a valid function argument" % node_name)
      return self.get_function_ref(node)
    except RuntimeError:
      return None
  
  def python_fn_to_parakeet(self, python_fn, src_info = None):
    if python_fn in AutoTranslate:
      python_fn = AutoTranslate[python_fn]
    if python_fn in ParakeetOperators:
      return ast_prim(ParakeetOperators[python_fn])
    else:
      return build_var(global_fn_name(python_fn), src_info)
  
  

  
  def visit_expr(self, node):
    print "Visit_expr", node 
    src_info = self.build_src_info(node)

    if isinstance(node, ast.Name):
      return build_var(node.id, src_info)
    elif isinstance(node, ast.BinOp):
      left = self.visit_expr(node.left)
      right = self.visit_expr(node.right)
      op_name = name_of_ast_node(node.op)
      print
      print 
      print node, node.op,  node.left, node.right  
      return build_prim_call(op_name, [left, right], src_info)
    elif isinstance(node, ast.BoolOp):
      args = [self.visit_expr(v) for  v in node.values]
      return build_prim_call(name_of_ast_node(node.op), args, src_info)
    elif isinstance(node, ast.UnaryOp):
      arg = self.visit_expr(node.operand)
      return build_prim_call(name_of_ast_node(node.op), [arg], src_info)
    elif isinstance(node, ast.Compare):
      assert len(node.ops) == 1
      
      #Not sure when there are multiple ops or multiple comparators?
      assert len(node.comparators) == 1
      op_name = name_of_ast_node(node.ops[0])
      left = self.visit_expr(node.left)
      right = self.visit_expr(node.comparators[0])

      return build_prim_call(op_name, [left, right], src_info)
    elif isinstance(node, ast.Subscript):
      op_name = name_of_ast_node(node.slice) 
      # only Slice and Index supported 
      assert op_name != "Ellipsis" and op_name != "ExtSlice" 
      return build_prim_call(op_name, args, src_info)
    elif isinstance(node, ast.Index):
      raise RuntimeError("[Parakeet] Unexpected index node in AST")
    elif isinstance(node, ast.Num):
      return build_num(node.n, src_info)
    elif isinstance(node, ast.Call):
      # neither var-args or dictionaries of keyword args are
      # supported by Parakeet since they make program 
      # analysis way harder
      assert node.starargs is None
      assert node.kwargs is None 
      fn = node.func
      args = node.args
      kwds = node.keywords 
      return self.visit_call(fn, args, kwds, src_info)
    else:
      raise RuntimeError("[Parakeet] AST node %s not supported " % type(node).__name__)
      return None

  def visit_call(self, fn, args, kwds, src_info = None):
    print "visit_call", fn, args, kwds 
    fn_name_parts = flatten_var_attrs(fn)
    assert len(fn_name_parts) > 0 
    base_name = fn_name_parts[0]
    # TODO: should be a check whether it's a local, not an arg 
    if base_name in self.arg_names:
      
      #Is the function an argument?
      #If so, we must be calling a numpy method on an array 
      assert len(fn_name_parts) == 1
      method_name = fn_name_parts[1]
      parakeet_obj =  build_var(base_name, src_info)
      self.visit_method_call(method_name, parakeet_obj, src_info)
    else:
      python_fn = self.get_function_ref(fn)
      if python_fn in Adverbs:
        assert len(args) > 1
        return self.visit_adverb(python_fn, args[0], args[1:],  kwds)
      else:
        return self.visit_simple_call(python_fn, args, kwds, src_info)
  
  def visit_method_call(self, method_name, parakeet_obj, src_info = None):
    if method_name in NumpyArrayMethods:
      prim_name = NumpyArrayMethods[method_name]
      return build_prim_call(prim_name, [parakeet_obj],  src_info)
    else:
      raise ParakeetUnsupported("Can't call method %s" % method_name)
    
    
  def visit_adverb(self, adverb, fn, args, kwds):
    python_fn = self.get_function_ref(fn)
    parakeet_fn = self.python_fn_to_parakeet(python_fn)
    parakeet_args = [parakeet_fn]
    for arg in args:
      parakeet_args.append(self.visit_expr(arg))

    parakeet_keywords = {}
    for pair in kwds:
      parakeet_keywords[pair.arg] = self.visit_expr(pair.value)
    assert adverb in ParakeetOperators 
    parakeet_adverb = ast_prim(ParakeetOperators[adverb])
    print
    print 
    print "building call from visit_adverb", parakeet_adverb, parakeet_args, parakeet_keywords
    return build_call(parakeet_adverb, parakeet_args, parakeet_keywords) 
    
  def visit_simple_call(self, python_fn, args, kwds, src_info=None):
    """
    A simple call is neither a ufunc method nor an adverb
    """
    print "visit_simple_call", python_fn, args, kwds 
    # have to handle arrays differently since they can contain
    # lists that otherwise are illegal in Parakeet programs
    if python_fn == np.array:
      # keywords on arrays not yet supported
      assert len(kwds) == 0
      assert len(args) == 1
      return self.visit_array_elts(args[0])
    else:
      parakeet_fn = self.python_fn_to_parakeet(python_fn)
      parakeet_args = [self.visit_expr(arg) for arg in args]
      parakeet_keywords = {}
      for pair in kwds:
        parakeet_keywords[pair.arg] = self.visit_expr(pair.value)
      return build_call(parakeet_fn, parakeet_args, parakeet_keywords) 
      
    
  def visit_array_elts(self, node):
    if isinstance(node, ast.List) or isinstance(node, ast.Tuple):
      elts = [self.visit_expr(elt) for elt in node.elts]
      return build_parakeet_array(elts, self.build_src_info(node)) 
    else:
      raise ParakeetUnsupported('Array must have literal arguments') 

  def build_src_info(self, node):
    #Temporary to fix seg faults:
    return None 
    try:
      file_name = c_char_p(self.file_name)
      line = c_int(self.line_offset + node.lineno)
      col = c_int(node.col_offset)
      return _source_info_t(_c_source_info_t(file_name, line, col))
    except AttributeError:
      return _source_info_t(None)

def ast_to_str(node, annotate_fields=True, include_attributes=False, indent='  '):
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

###############################################################################
#  Function Registration`
###############################################################################



import inspect 
def register_function(f):
  print  
  print "Registering", f
  print 
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
      print 
      print "...found other_fn", other_fn
      print  
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
  
  print 
  print "ARGS", positional, default_args 
  print 
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
  return fun_id, global_vars
