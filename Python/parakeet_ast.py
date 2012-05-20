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
adverbs = [parakeet_lib.map, parakeet_lib.reduce,
           parakeet_lib.allpairs, parakeet_lib.scan]

#Keeps track of the user-made functions that have been made and the built-ins
VisitedFunctions = ParakeetOperators.copy()
VisitedFunctions[np.array] = ''

###############################################################################
#  Helper functions
###############################################################################

def build_fn_node(src_info, python_fn):
  if python_fn in ParakeetOperators:
    parakeet_prim_name = ParakeetOperators[python_fn]
    parakeet_fn = ast_prim(parakeet_prim_name)
    if parakeet_prim_name is None:
      raise RuntimeError("[Parakeet] Support for %s not implemented" %
                         python_fn)
  elif python_fn in NumpyArrayMethods:
    parakeet_prim_name = NumpyArrayMethods[python_fn]
    parakeet_fn = ast_prim(parakeet_prim_name)
    if parakeet_prim_name is None:
      raise RuntimError("[Parakeet] Support for %s not implemented" %
                        python_fn)
  elif python_fn in NumpyArrayAttributes:
    parakeet_prim_name = NumpyArrayAttributes[python_fn]
    parakeet_fn = ast_prim(parakeet_prim_name)
    if parakeet_prim_name is None:
      raise RuntimError("[Parakeet] Support for %s not implemented" %
                        python_fn)
  else:
    c_name = c_char_p(global_fn_name(python_fn))
    parakeet_fn = LibPar.mk_var(c_name, src_info.addr)
  return parakeet_fn

#Function to get ast node(s) for built-in functions/primitives
def ast_prim(sym):
  return c_void_p(LibPar.get_prim(sym))

    
class ParakeetUnsupported(Exception):
  def __init__(self, value):
    self.value = value
  def __str__(self):
    return repr(self.value)

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

def name_of_ast_node(op):
  return op.__class__.__name__


def src_addr(src_info):
  if src_info is None:
    return 0
  else:
    return src_info.addr
  
def mk_return(elts, src_info=None):
  arr = list_to_ctypes_array(elts) if len(elts) > 0 else None
  return LibPar.mk_return(arr, len(elts), src_addr(src_info))

def mk_array(elts, src_info = None):
  arr = list_to_ctypes_array(elts)
  return LibPar.mk_array(arr, len(elts), src_addr(src_info))

def mk_block(stmts, src_info = None):
  arr = list_to_ctypes_array(stmts)
  return LibPar.mk_block(arr, len(stmts), src_addr(src_info))


def mk_call(fn, positional_args, kw_names = [], kw_values = [], src_info=None):
  """
    lower-level helper for building a function call, 
    Assumes function is a Parakeet node and that keyword
    names have already been converted to character pointers
  """
  assert len(kw_names) == len(kw_values)
  return LibPar.mk_call(
    fn,  list_to_ctypes_array(positional_args), len(positional_args), 
    list_to_ctypes_array(kw_names), list_to_ctypes_array(kw_values), 
    len(kw_names), 
    src_addr(src_info))

def build_call(self, python_fn, args, kw_args = {}, src_info = None):
  """
    higher-level helper for building a function call. 
    Assumes that the function is a Python object which needs
    to be translated into a Parakeet node 
  """ 
  parakeet_fn = build_fn_node(src_info, python_fn)
  kw_names = []
  kw_values = []
  for k,v in kw_args.items():
    kw_names.append(c_char_p(k))
    kw_values.append(v)
  return mk_call(parakeet_fn, args, kw_names, kw_values, src_info)
   

def build_prim_call(python_op_name, args, src_info = None):
  """
    Given the Python AST name for a primitive operator, 
    translate it to the equivalent Parakeet primitive and
    create a Call node for that primitive
  """
  parakeet_op_name = BuiltinPrimitives[python_op_name]
  if parakeet_op_name is None:
    raise RuntimeError('Prim not implemented: %s' % python_op_name)
  else:
    prim = ast_prim(parakeet_op_name)
  return mk_call(prim, args, src_info = src_info)

def build_var(name, src_info = None):
  #Special case for booleans
  if name == 'True':
    return LibPar.mk_bool_paranode(1, src_addr(src_info))
  elif name == 'False':
    return LibPar.mk_bool_paranode(0, src_addr(src_info))
  else:
    return LibPar.mk_var(c_char_p(name), src_addr(src_info))

def build_num(num, src_info = None):
  """
  Given the string form of a number, build a syntax node for an int or float
  """
  #num = eval(str_num)
  if type(num) == int:
    return LibPar.mk_int32_paranode(num, src_addr(src_info))
  elif type(num) == float:
    return LibPar.mk_float_paranode(c_float(num), src_addr(src_info))
  else:
    raise RuntimeError("Failed to create number from " + str_num)




###############################################################################
#  Converter
###############################################################################
class ASTConverter():
  def __init__(self, global_refs, arg_names, file_name, line_offset):
    self.seen_functions = set([])
    self.arg_names = arg_names
    self.global_variables = set()
    self.file_name = file_name
    self.line_offset = line_offset
    self.global_refs = global_refs


  def get_function_ref(self, node):
    if isinstance(node, ast.Name):
      funName = node.id
      if funName in self.global_refs:
        funRef = self.global_refs[funName]
      elif funName in __builtins__:
        funRef = __builtins__[funName]
      else:
        raise RuntimeError("[Parakeet] Couldn't find:" + funName)
    elif isinstance(name, ast.Attribute):
      #For function calls like mod1.mod2.mod4.fun()
      moduleList = []
      nextNode = node
      funName = node.attr
      #Get a list of the chain of modules

      while instance(nextNode, ast.Attribute):
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
            raise RuntimeError("[Parakeet] %s is invalid?" %
                               moduleList.join('.'))
          else:
            raise RuntimeError("[Parakeet] Invalid object %s" % currModule)
      funRef = currModule
    else:
      raise RuntimeError("[Parakeet] Call.func shouldn't be", name)
    
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
      node_name = self.get_global_var_name(node)
      if node_name.split('.')[0] in self.arg_names and (
        len(node_name.split('.')) > 1):
        raise "[Parakeet] %s is not a valid function argument" % node_name
      return self.get_function_ref(node)
    except RuntimeError:
      return None



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
    
  
  def visit_stmt_sequence(self, stmts):
    print "visit_stmt_seq"
    return mk_block([self.visit_stmt(stmt) for stmt in stmts])

  def visit_stmt(self, node, src_info = None):
    print "visit_stmt", node
    srcAddr = src_addr(src_info)
    if isinstance(node, ast.If):
      test = self.visit_expr(test)
      if_true = self.visit_stmt_sequence(node.body, src_info)
      if_false = self.visit_stmt_sequence(node.orelse, src_info)
      return LibPar.mk_if(test, if_true, if_false, srcAddr)
    elif isinstance(node, ast.Assign):
      return self.visit_assign(node)
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
  
  def visit_return(self, node, src_info = None):
    if node.value is None:
      values = []
    elif isinstance(node.value, ast.Tuple): 
      values = [self.visit_expr(v) for v in node.value.elts]
    else:
      values = [self.visit_expr(node.value)]
    print values 
    return mk_return(values, src_info)
  def visit_call(self, node):
    node_name = self.get_global_var_name(node.func)
    if not node_name.split('.')[0] in self.arg_names:
      funRef = self.get_function_ref(node.func)
      if funRef is None:
        raise RuntimeError("[Parakeet] Expected %s to be a function" % node.func)
      if hasattr(funRef, '__self__') and funRef.__self__:
        if funRef.__self__ in ValidObjects:
          func = ValidObjects[funRef.__self__]
          if func in AutoTranslate:
            func = AutoTranslate[func]
          self.seen_functions.add(func)
          fun_name = global_fn_name(func)
          par_name = LibPar.mk_var(c_char_p(fun_name), src_info.addr)
          fun_arg = par_name
          
          kw_arg_names = ['fixed']
          kw_arg_values = []  
          
          if funRef.__name__ == "reduce" or "accumulate":
            arr_args = self.build_arg_list([node.args[0]], contextSet)
            kw_arg_names.append('default')
            kw_arg_values.append(LibPar.mk_none(None))
            if len(node.args) == 1:
              
              kw_args['axis'] = LibPar.mk_int32_paranode(0, src_info.addr)
            elif len(node.args) == 2:
              kw_args['axis'] = self.visit(node.args[1], contextSet)
            else:
              raise RuntimeError("TOO MANY ARGUMENTS FOR %s" % funRef.__name__)
              para_arr_args = self.build_parakeet_array(src_info, arr_args)
              args = [fun_arg]
              args.append(para_arr_args)
              args.append(kw_args['fixed'])
              args.append(kw_args['axis'])
              args.append(kw_args['default'])
              if funRef.__name__ == "reduce":
                return self.build_call(src_info, parakeet_lib.reduce, args)
              elif funRef.__name__ == "accumulate":
                return self.build_call(src_info, parakeet_lib.scan, args)
              else:
                assert False
        else:
          childContext = set(contextSet)
          if funRef == np.array:
            childContext.add('array')
            assert len(node.args) == 1
            return self.visit(node.args[0], childContext)
          elif funRef in adverbs:
            fun_arg = self.build_arg_list([node.args[0]], childContext)
            arr_args = self.build_arg_list(node.args[1:], childContext)
            kw_args = {'fixed': self.build_parakeet_array(src_info,[]),
                       'axis': LibPar.mk_none(None)
                      }
            if funRef == parakeet_lib.reduce:
              kw_args['default'] = LibPar.mk_none(None)
            childContext.add('rhs')
            childContext.add('array')
            for kw_arg in node.keywords:
              kw = kw_arg.arg
              val = kw_arg.value
              if type(val).__name__ == 'List':
                val_args = []
                for v_arg in val.elts:
                  val_args.append(self.visit(v_arg, childContext))
                kw_args[kw] = self.build_parakeet_array(src_info,val_args)
              else:
                val = self.visit(kw_arg.value, childContext)
                kw_args[kw] = self.build_parakeet_array(src_info,[val])
            args = fun_arg
            para_arr_args = self.build_parakeet_array(src_info,arr_args)
            args.append(para_arr_args)
            args.append(kw_args['fixed'])
            args.append(kw_args['axis'])
            if funRef == parakeet_lib.reduce:
              args.append(kw_args['default'])
            return self.build_call(src_info, funRef, args)
          else:
            funArgs = self.build_arg_list(node.args, childContext)
            return self.build_call(src_info, funRef, funArgs)
      #If it's a local variable
      else:
        method = '.'.join(node_name.split('.')[1:])
        if (method in NumpyArrayMethods):
          var = node_name.split('.')[0]
          var_node = self.build_var(src_info, var)
          return self.build_call(src_info, method, [var_node])
  
  
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
      return build_prim_call(op_name, args, src_info)
    elif isinstance(node, ast.Index):
      raise RuntimeError("[Parakeet] Unexpected index node in AST")
    elif isinstance(node, ast.Num):
      return build_num(node.n, src_info)

    else:
      raise RuntimeError("[Parakeet] %s not supported " % node)
      return None

  
  def visit_array_elts(self, node):
    if isinstance(node, ast.List) or isinstance(node, ast.Tuple):
      elts = [self.visit_expr(elt) for elt in node.elts]
      return build_parakeet_array(elts, self.build_src_info(node)) 
    else:
      raise ParakeetUnsupported('Array must have literal arguments') 

  def build_arg_list(self, python_nodes,  contextSet):
    results = []
    for node in python_nodes:
      funRef = self.register_if_function(node)
      if funRef is not None:
        src_info = self.build_src_info(node)
        results.append(build_fn_node(src_info, funRef))
      else:
        result = self.visit(node, contextSet)
        if result is not None:
          results.append(result)
    return results


  def build_src_info(self, node):
    #Temporary to fix seg faults:
    return _source_info_t(None)
    try:
      file_name = c_char_p(self.file_name)
      line = c_int(self.line_offset + node.lineno)
      col = c_int(node.col_offset)
      return _source_info_t(_c_source_info_t(file_name, line, col))
    except AttributeError:
      return _source_info_t(None)

  def get_global_var_name(self, node):
    var_str = ""
    currNodeType = type(node).__name__
    if not currNodeType in ('Name','Attribute'):
      raise RuntimeError("[Parakeet] %s name fetching not supported" %
                         currNodeType)
    currNode = node
    while (currNodeType != "Name"):
      var_str = "." + currNode.attr + var_str
      currNode = currNode.value
      currNodeType = type(currNode).__name__
      if currNodeType not in ('Name','Attribute'):
        raise RuntimeError("[Parakeet] Accessing with %s not supported" %
                           currNodeType)
    var_str = currNode.id + var_str
    return var_str


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

  default_values = [] if argspec.defaults is None else argspec.defaults
  n_defaults = len(default_values)
  positional = argspec.args[:-n_defaults]
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
  return fun_id, global_vars
