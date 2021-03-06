import ast, math
import numpy as np
from ctypes import *
import parakeet_common
from parakeet_common import LibPar, LOG, list_to_ctypes_array
from parakeet_source_info import _c_source_info_t, _source_info_t, src_addr
import parakeet_adverbs


###############################################################################
#  Global variables
###############################################################################

# mapping from Python functions to builtin Parakeet primitives
ParakeetOperators = {
  np.add:'add',
  abs: 'abs',
  np.arange: 'range',
  math.exp:'exp',
  math.log:'log',
  math.pow:'pow',
  math.sqrt:'sqrt',
  np.size:'size',
  parakeet_adverbs.map:'map',
  parakeet_adverbs.reduce:'reduce',
  parakeet_adverbs.scan:'scan',
  parakeet_adverbs.allpairs:'allpairs',
}

# mapping from Python functions to other Python functions wrapped by Parakeet
AutoTranslate = {}

# mapping from Python primitive operators to buitin Parakeet primitives
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

# mapping from methods of numpy arrays to Parakeet primitive operators
# ==> THESE ARE THE ONLY METHODS YOU CAN USE IN PARAKEET CODE! 
NumpyArrayMethods = {
  "transpose": "transpose",
  "flatten": "flatten",
  "copy": "copy",
}
NumpyArrayAttributes = {
  "shape": "shape",
  "strides": "strides",
}

Adverbs = [parakeet_adverbs.map, parakeet_adverbs.reduce,
           parakeet_adverbs.allpairs, parakeet_adverbs.scan]


###############################################################################
#  Helper functions
###############################################################################

def build_none(src_info = None):
  return LibPar.mk_none(src_addr(src_info))

def build_bool(b, src_info = None):
  if b: 
    return LibPar.mk_bool_paranode(1, src_addr(src_info))
  else:
    return LibPar.mk_bool_paranode(0, src_addr(src_info))

def build_var(name, src_info = None):
  #Special case for booleans and None
  if name == 'True':
    return build_bool(True, src_info)
  elif name == 'False':
    return build_bool(False, src_info)
  elif name == 'None':
    return mk_none(src_info)
  else:
    return LibPar.mk_var(c_char_p(name), src_addr(src_info))

def build_int(i, src_info = None):
  return LibPar.mk_int32_paranode(i, src_addr(src_info))

def build_long(l, src_info = None): 
  return LibPar.mk_int64_paranode(l, src_addr(src_info))

def build_float(f, src_info = None):  
  return LibPar.mk_double_paranode(c_double(f), src_addr(src_info))

def build_num(num, src_info = None):
  """
  Given the string form of a number, build a syntax node for an int or float
  """
  num_type = type(num)
  if num_type == int:
    return build_int(num, src_info)
  elif num_type == long:
    return build_long(num, src_info)    
  elif num_type == float:
    return build_float(num, src_info)
  else:
    raise ParakeetUnsupported("Unsupported numeric type " + num_type)

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
  n_args = c_int(len(positional_args))
  kw_names_array = list_to_ctypes_array(kw_names, c_char_p)
  kw_values_array = list_to_ctypes_array(kw_values)
  n_kwds = c_int(len(kw_names))
  srcAddr = src_addr(src_info)
  
  #print "mk_call", kw_names_array, kw_values_array   
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

def mk_tuple(elts, src_info = None):
  arr = list_to_ctypes_array(elts)
  return LibPar.mk_tuple(arr, len(arr), src_addr(src_info))

def mk_block(stmts, src_info = None):
  arr = list_to_ctypes_array(stmts)
  return LibPar.mk_block(arr, len(stmts), src_addr(src_info))



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
    body = node.body[0]
    print body 
    ptr = self.visit_function_def(node.body[0])
    print "ptr", ptr
    return ptr 
  
  # TODO: don't ignore function decorators!  
  def visit_function_def(self, node):
    #print "visit_function_def"
    assert isinstance(node, ast.FunctionDef)
    # we don't support variable argument functions
    assert node.args.vararg is None
    # we don't support dictionaries of keyword arguments  
    assert node.args.kwarg is None
    # assume the function arguments have been pulled out elsewhere and 
    # we just need the body 

    return self.visit_stmt_sequence(node.body)
    
  
  def visit_stmt_sequence(self, stmts, src_info=None):
    stmt_nodes = [self.visit_stmt(stmt) for stmt in stmts]
    print stmt_nodes
    return mk_block(stmt_nodes, src_info)

  def visit_stmt(self, node, src_info = None):
    #print "visit_stmt", node
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
      assert node.orelse == [] or node.orelse is None
      block = self.visit_stmt_sequence(node.body)
      test = self.visit_expr(node.test)
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
    return LibPar.mk_assign(list_to_ctypes_array(vars), len(vars), rhs, src_info)
       
  def visit_return(self, node, src_info = None):
    if node.value is None:
      values = []
    elif isinstance(node.value, ast.Tuple): 
      values = [self.visit_expr(v) for v in node.value.elts]
    else:
      values = [self.visit_expr(node.value)]
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
          if currModule in AutoTranslate:
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
  

  def get_slice_children(self, node): 
    if isinstance(node, ast.Index):
      if isinstance(node.value, ast.Tuple):
        return [self.visit_expr(v) for v in node.value.elts]
      else:
        return [self.visit_expr(node.value)]
    elif isinstance(node, ast.Slice):
      def helper(child):
        if child is None: 
          return mk_none()
        else:
          return self.visit_expr(child)
      return map(helper, [node.lower, node.upper, node.step])
    else:
      raise ParakeetUnsupported("Slice of type " + str(type(node)) + " not supported") 
      
  def visit_expr(self, node):
    src_info = self.build_src_info(node)

    if isinstance(node, ast.Name):
      return build_var(node.id, src_info)
    elif isinstance(node, ast.BinOp):
      left = self.visit_expr(node.left)
      right = self.visit_expr(node.right)
      op_name = name_of_ast_node(node.op)
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
      arr = self.visit_expr(node.value)
      indexArgs = self.get_slice_children(node.slice)
      args = [arr] + indexArgs
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
    elif isinstance(node, ast.Tuple):
       elts = [self.visit_expr(elt) for elt in node.elts]
       return mk_tuple(elts, src_info)
    else:
      raise RuntimeError("[Parakeet] AST node %s not supported " % type(node).__name__)
      return None

  def visit_call(self, fn, args, kwds, src_info = None):
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
    return build_call(parakeet_adverb, parakeet_args, parakeet_keywords) 
    
  def visit_simple_call(self, python_fn, args, kwds, src_info=None):
    """
    A simple call is neither a ufunc method nor an adverb
    """
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
      return mk_array(elts, self.build_src_info(node)) 
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

  
