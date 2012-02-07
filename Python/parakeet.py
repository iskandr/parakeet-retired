from ctypes import *
import ast, math, os, sys
import functools as ft
import numpy as np
import para_libs
import PrettyAST

###############################################################################
#  Initializations
###############################################################################
verbose = True
debug = False
def LOG(msg):
  if verbose: print "[Parakeet]", msg

#Return type struct
class _ret_scalar_value(Union):
  _fields_ = [("boolean", c_int),
              ("int32", c_int),
              ("int64", c_int64),
              ("float32", c_float),
              ("float64", c_double)]

class _scalar_ret_t(Structure):
  _fields_ = [("ret_type", c_void_p),
              ("ret_scalar_value", _ret_scalar_value)]

class _array_ret_t(Structure):
  _fields_ = [("ret_type", c_void_p),
              ("data", c_void_p),
              ("shape", c_void_p),
              ("shape_len", c_int),
              ("strides", c_void_p),
              ("strides_len", c_int)]

class _ret_data(Union):
  _fields_ = [("array", _array_ret_t),
              ("scalar", _scalar_ret_t)]

class _ret_t(Structure):
  _fields_ = [("data", _ret_data),
              ("is_scalar", c_int)]

class return_val_t(Structure):
  _fields_ = [("return_code", c_int),
              ("results_len", c_int),
              ("error_msg", c_char_p),
              ("results", POINTER(_ret_t))]

#Return type initialization
#Small fix: Can set default return type to c_void_p?
def return_type_init(LibPar):
  LibPar.parakeet_init()
  #NOTE: can set default to c_void_p, much less initialization?
  LibPar.mk_array.restype = c_void_p
  LibPar.mk_assign.restype = c_void_p
  LibPar.mk_int32_paranode.restype = c_void_p
  LibPar.mk_int64_paranode.restype = c_void_p
  LibPar.mk_var.restype = c_void_p
  LibPar.mk_app.restype = c_void_p
  LibPar.mk_lam.restype = c_void_p
  LibPar.mk_block.restype = c_void_p
  LibPar.mk_host_array.restype = c_void_p
  LibPar.register_untyped_function.restype = c_int
  LibPar.run_function.restype = return_val_t
  LibPar.mk_int32.restype = c_void_p
  LibPar.mk_int64.restype = c_void_p
  LibPar.mk_float32.restype = c_void_p
  LibPar.mk_float64.restype = c_void_p
  LibPar.mk_whileloop.restype = c_void_p
  LibPar.mk_bool.restype = c_void_p
  LibPar.mk_bool_paranode.restype = c_void_p
  LibPar.get_prim.restype = c_void_p
  LibPar.mk_float_paranode.restype = c_void_p
  LibPar.mk_double_paranode.restype = c_void_p
  LibPar.mk_bool.restype = c_void_p
  LibPar.mk_void.restype = c_void_p
  LibPar.mk_return.restype = c_void_p
  LibPar.print_ast_node.restype = c_void_p

  #get global values for parakeet types
  LibPar.bool_t = c_int.in_dll(LibPar, "parakeet_bool_elt_t").value
  LibPar.char_t = c_int.in_dll(LibPar, "parakeet_char_elt_t").value
  LibPar.int32_t = c_int.in_dll(LibPar, "parakeet_int32_elt_t").value
  LibPar.int64_t = c_int.in_dll(LibPar, "parakeet_int64_elt_t").value
  LibPar.float32_t = c_int.in_dll(LibPar, "parakeet_float32_elt_t").value
  LibPar.float64_t = c_int.in_dll(LibPar, "parakeet_float64_elt_t").value

#Function to get ast node(s) for built-in functions/primitives
def ast_prim(sym):
  return c_void_p(LibPar.get_prim(sym))

#Load libraries, basic initialization
LibPar = cdll.LoadLibrary(os.getenv('HOME') + '/.parakeet/libparakeetpy.so')
return_type_init(LibPar)

###############################################################################
#  Global variables
###############################################################################

SafeFunctions = {
  np.add:'add',
  abs: 'abs',
  np.arange: 'range',
  math.exp:'exp', 
  math.log:'log', 
  math.pow:'pow', 
  math.sqrt:'sqrt', 
  para_libs.map:'map', 
  para_libs.reduce:'reduce',
}

BuiltinPrimitives = {
  'Add':'add', 
  'Sub':'sub', 
  'Mult':'mult', 
  'Div':'div', 
  'Mod':'mod', 
  'Pow':None, 
  'LShift':None, 
  'RShift':None, 
  'BitOr':None, 
  'BitXor':None,
  'BitAnd':None,
  'FloorDiv':None,
  'Invert':None, 
  'Not':None, 
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
#Keeps track of the user-made functions that have been made and the built-ins
VisitedFunctions = SafeFunctions.copy()

#global variables referenced by each function, for when they are called
#not used yet
FunctionGlobals = {}

NumpyTypeToCtype = {
  np.int32: c_int32,
  np.int64: c_int64,
  np.float32: c_float,
  np.float64: c_double,
  np.bool: c_bool,
}

NumpyTypeToParakeetType = {
  np.int32: LibPar.int32_t,
  np.int64: LibPar.int64_t,
  np.float32: LibPar.float32_t,
  np.float64: LibPar.float64_t,
  np.bool: LibPar.bool_t
}

ParakeetTypeToCtype = {
  LibPar.int32_t: c_int32,
  LibPar.int64_t: c_int64,
  LibPar.float32_t: c_float,
  LibPar.float64_t: c_double,
  LibPar.bool_t: c_int,
  LibPar.char_t: c_char
}

###############################################################################
#  Helper functions
###############################################################################
#Builds a ctypes list of the type out of the input_list
def list_to_ctypes_array(input_list, t):
  print "[list_to_ctypes_array] input:", input_list
  n = len(input_list)
  ARRAY_TYPE = t * n # Description of a ctypes array
  arr = ARRAY_TYPE()
  for i in range(n):
    arr[i] = input_list[i]
  print "[list_to_ctypes_array] output:", arr
  return arr

def read_file(file_name):
  try:
    f = open(file_name,'r')
    out_string = f.readlines()
    f.close()
  except IOError:
    print "Error opening %s" % file_name
    return ''
  return out_string

def function_info(function_obj):
  try:
    codeInfo = function_obj.__code__
  except:
    return ''
  args = codeInfo.co_varnames[:codeInfo.co_argcount]
  variableList = list(codeInfo.co_varnames)
  #Get actual text of the file containing the function
  codeText = read_file(str(codeInfo.co_filename))
  #Keeps track of current line number
  lineNo = 1
  #Keeps track of size of tab
  sizeTab = 0
  #Tabs can be spaces or \t
  tabChar = ' '
  #Will only contain code from inside the function
  outString = ""
  #Keeps track of blank/comment lines/space between decorator and function
  extraLines = 0
  for line in codeText:
    if lineNo == codeInfo.co_firstlineno + extraLines:
      try:
        if line.split()[0] == 'def':
          pass
        else:
          extraLines += 1
      except:
        extraLines += 1
    if lineNo == codeInfo.co_firstlineno+1+extraLines:
      if line.isspace() or line.strip()[0] == '#':
        extraLines += 1
    if lineNo > codeInfo.co_firstlineno + extraLines:
      #Special case for first line of code
      if lineNo == codeInfo.co_firstlineno + 1 + extraLines:
        tabChar = line[0]
        for c in line:
          if c == tabChar:
            sizeTab += 1
          else:
            break
      if line.isspace():
        pass
      elif line[:sizeTab] == tabChar * sizeTab:
        outString += line[sizeTab:]
      elif line[:sizeTab].__contains__('#'):
        pass
      else:
        break
    lineNo += 1
  return args, outString, variableList

class ParakeetUnsupported(Exception):
  def __init__(self, value):
    self.value = value
  def __str__(self):
    return repr(self.value)

# always assume functions have a module but
def global_fn_name(fn, default_name="<unknown_function>"):
  if hasattr(fn, '__name__'):
    return fn.__module__ + "." + fn.__name__
  else:
    return fn.__module__ + "." + default_name

def build_parakeet_array(elts):
  c_array = list_to_ctypes_array(elts, c_void_p)
  return LibPar.mk_array(c_array, len(elts), None)

def build_parakeet_block(stmts):
  c_array = list_to_ctypes_array(stmts, c_void_p)
  return LibPar.mk_block(c_array, len(stmts), None)

def build_call(python_fn, args):
  if python_fn in SafeFunctions:
    parakeet_prim_name = SafeFunctions[python_fn]
    parakeet_fn = ast_prim(parakeet_prim_name)
    if parakeet_prim_name is None: 
      raise RuntimeError("[Parakeet] Support for %s not implemented" % python_fn)
  else:
    c_name = c_char_p(global_fn_name(python_fn))
    parakeet_fn = LibPar.mk_var(c_name, None)
  parakeet_args = list_to_ctypes_array(args,c_void_p)
  n = len(parakeet_args)
  return LibPar.mk_app(parakeet_fn, parakeet_args, n, None)

def build_prim_call(python_op_name, *args):
  LOG("build_prim_call(%s, %s)" % (python_op_name, args))
  parakeet_args = list_to_ctypes_array(args,c_void_p)
  parakeet_op_name = BuiltinPrimitives[python_op_name]
  if parakeet_op_name is None: 
    raise RuntimeError('Prim not implemented: %s' % python_op_name)
  else: 
    prim = ast_prim(parakeet_op_name) 
  return LibPar.mk_app(prim, parakeet_args, len(parakeet_args), None)

def name_of_ast_node(op): 
  return op.__class__.__name__ 

def build_var(name): 
  #Special case for booleans
  if name == 'True':
    return LibPar.mk_bool_paranode(1, None)
  elif name == 'False':
    return LibPar.mk_bool_paranode(0, None)
  else:
    return LibPar.mk_var(c_char_p(name), None)

def build_num(str_num):
  """
  Given the string form of a number, build a syntax node for an int or float
  """
  num = eval(str_num)
  LOG("%s(%s)" %(type(num), num))
  if type(num) == int:
    return LibPar.mk_int32_paranode(num,None)
  elif type(num) == float:
    return LibPar.mk_float_paranode(c_float(num),None)
  else:
    assert False

def build_parakeet_node(node, args):
  """Build simple case syntax nodes which don't involve function arguments""" 
  #args is the children nodes in the correct type (i.e. node or literal)
  node_type = name_of_ast_node(node)
  # either variable name or bool literal
  print "build_parakeet_node", node, args
  if node_type == 'Name':
    return build_var(args[0])

  elif node_type == 'Assign':
    print "Assign args", args
    LOG("assign(%s, %s)" % (args[0][0], args[1]))
    lhs_list = args[0]
    lhs_array_ptr = list_to_ctypes_array(lhs_list, c_void_p)
    num_lhs_ids = len(lhs_list)
    rhs = args[1]
    return LibPar.mk_assign(lhs_array_ptr, num_lhs_ids, rhs, 0)
    #Mk a def where the target is the index node
  elif node_type == 'BinOp':
    return build_prim_call(name_of_ast_node(node.op), args[0], args[2])
  elif node_type == 'UnaryOp':
    return build_prim_call(name_of_ast_node(node.op), args[1])
  elif node_type == 'Compare':
    #Not sure when there are multiple ops or multiple comparators?
    return build_prim_call(name_of_ast_node(node.ops[0]), args[0], args[2][0])
  elif node_type == 'Subscript':
    return build_prim_call(name_of_ast_node(node.slice), args[0], args[1])
  elif node_type == 'Index':
    LOG("Index %s" % str(args))
    assert False
    #return args[0]
  elif node_type == 'Num':
    return build_num(args[0])
  elif node_type == 'Call':
    raise RuntimeError ('[Parakeet] Unexpected Call node')
  elif node_type == 'Module':
    LOG("block(%s)" % str(args))
    return build_parakeet_block(args[0])
  elif node_type == 'If':
    LOG("if(%s, %s, %s)" % (args[0], args[1], args[2]))
    thenBlock = build_parakeet_block(args[1])
    elseBlock = build_parakeet_block(args[2])
    return LibPar.mk_if(args[0], thenBlock, elseBlock, None)
  elif node_type == 'While':
    LOG("while(%s, %s)" % (args[0], args[1]))
    block = build_parakeet_block(args[1])
    return LibPar.mk_whileloop(args[0],block,None)
  elif node_type == 'Attribute':
    LOG("Attribute %s " % str(args))
    #NOTE: doesn't make ANY sense right now
    #return args[1]
    assert False
  else:
    print "[Parakeet]", node_type, "with args", str(args), "not handled"
    return None


###############################################################################
#  Converter
###############################################################################
class ASTConverter():
  def __init__(self, global_vars, arg_names):
    self.seen_functions = set([]) 
    self.arg_names = arg_names
    self.global_variables = global_vars

  # currContext is a set of strings telling us which nodes are parents
  # of the current one in the syntax tree
  def visit(self, node, contextSet=set([])):
    nodeType = type(node).__name__
    if nodeType == 'Print':
      raise ParakeetUnsupported("printing is not allowed")
    if 'lhs' in contextSet and nodeType == 'Attribute':
      raise ParakeetUnsupported("changing a field is not allowed")
    if 'rhs' in contextSet and nodeType == 'Str':
      raise ParakeetUnsupported("strings are not supported")
    elif nodeType == 'List' or nodeType == 'Tuple':
      if 'array' in contextSet:
        #('lhs' in contextSet and nodeType == 'Tuple'):
        children = ast.iter_child_nodes(node)
        elts = self.build_arg_list(children, contextSet)
        return build_parakeet_array(elts)
      else:
        raise ParakeetUnsupported(
            "lists and tuples are not supported outside of numpy arrays")
    elif nodeType == 'Call':
      funRef = self.get_function_ref(node.func)
      childContext = set(contextSet)
      if funRef == np.array:
        childContext.add('array')
        assert len(node.args) == 1
        return self.visit(node.args[0], childContext)

      elif funRef == para_libs.map:
        fun_arg = self.build_arg_list([node.args[0]], childContext)
        arr_args = self.build_arg_list(node.args[1:], childContext)
        kw_args = {'fixed': build_parakeet_array([]),
                   'axis': LibPar.mk_void(None)
                  }
        childContext.add('rhs')
        childContext.add('array')
        for kw_arg in node.keywords:
          kw = kw_arg.arg
          val = kw_arg.value
          if type(val).__name__ == 'List':
            val_args = []
            for v_arg in val.elts:
              val_args.append(self.visit(v_arg, childContext))
            kw_args[kw] = self.build_parakeet_array(val_args)
          else:
            val = self.visit(kw_arg.value, childContext)
            kw_args[kw] = build_parakeet_array([val])
        args = fun_arg
        para_arr_args = build_parakeet_array(arr_args)
        args.append(para_arr_args)
        args.append(kw_args['fixed'])
        args.append(kw_args['axis'])
        return build_call(funRef, args)
      elif funRef == para_libs.reduce:
        fun_arg = self.build_arg_list([node.args[0]], childContext)
        arr_args = self.build_arg_list(node.args[1:], childContext)
        print arr_args
        kw_args = {'fixed': build_parakeet_array([]),
                   'axis': LibPar.mk_void(None),
                   'default':LibPar.mk_void(None)
                  }
        childContext.add('rhs')
        childContext.add('array')
        for kw_arg in node.keywords:
          kw = kw_arg.arg
          val = kw_arg.value
          if type(val).__name__ == 'List':
            val_args = []
            for v_arg in val.elts:
              val_args.append(self.visit(v_arg, childContext))
            kw_args[kw] = build_parakeet_array(val_args)
          else:
            val = self.visit(kw_arg.value, childContext)
            kw_args[kw] = build_parakeet_array([val])
        args = fun_arg
        para_arr_args = build_parakeet_array(arr_args)
        args.append(para_arr_args)
        args.append(kw_args['fixed'])
        args.append(kw_args['axis'])
        args.append(kw_args['default'])
        return build_call(funRef, args)
      else:
        funArgs = self.build_arg_list(node.args, childContext)
        return build_call(funRef, funArgs)
    elif nodeType == 'Subscript':
      args = []
      args.append(self.visit(node.value, contextSet))
      slice_type = type(node.slice).__name__
      if slice_type == "Index":
        index_type = type(node.slice.value).__name__
        if index_type == "Tuple":
          for index_arg in node.slice.value.elts:
            args.append(self.visit(index_arg, contextSet))
        else:
          args.append(self.visit(node.slice.value, contextSet))
      else:
        raise ParakeetUnsupported(
            "slicing of type %s is not supported" % slice_type)
      return build_parakeet_node(node, args)
      #args[1]...[n+1] is what's inside the tuple, not the tuple itself
      #[args[0], args[1],....,args[n+1]]

    elif nodeType == 'Return':
      #Might not be right for no return
      if node.value is None:
        return LibPar.mk_return(None, 0,None)
      elif type(node.value).__name__ == "Tuple":
        children = ast.iter_child_nodes(node.value)
        elts = self.build_arg_list(children, contextSet)
        ret_args = list_to_ctypes_array(elts, c_void_p)
        return LibPar.mk_return(ret_args,len(elts),None)
      else:
        ret = self.visit(node.value, contextSet)
        ret_args = list_to_ctypes_array([ret], c_void_p)
        return LibPar.mk_return(ret_args,1,None)
    elif nodeType == 'Assign':
      print "In assign"
      leftChildContext = set(contextSet)
      rightChildContext = set(contextSet)
      leftChildContext.add('lhs')
      rightChildContext.add('rhs')
      if len(node.targets) != 1:
        #This shouldn't happen
        assert False
      if type(node.targets[0]).__name__ == "Tuple":
        children = ast.iter_child_nodes(node.targets[0])
        elts = self.build_arg_list(children, leftChildContext)
        lhs_args = list_to_ctypes_array(elts, c_void_p)
        num_args = len(elts)
      else:
        elt = self.visit(node.targets[0], leftChildContext)
        lhs_args = list_to_ctypes_array([elt], c_void_p)
        num_args = 1
        LibPar.print_ast_node(elt)
      rhs_arg = self.visit(node.value, rightChildContext)
      print "before mk_assign"
      return LibPar.mk_assign(lhs_args, num_args, rhs_arg, None)

    parakeetNodeChildren = []
    for childName, childNode in ast.iter_fields(node):
      #######################################################################
      #  Single AST Node
      #######################################################################
      if isinstance(childNode, ast.AST):
        # Don't need to visit the function node since
        # it will be handled directly by 'build_parakeet_node'
        if nodeType == 'Call' and childName == 'func': continue
        childContext = set(contextSet)
        # if you're a single child whose parent's nodeType is assign
        # then you're the RHS
        if nodeType == 'Assign':
          childContext.add('rhs')
        parakeetNodeChildren.append(self.visit(childNode,childContext))

      #######################################################################
      #  List of AST Nodes
      #######################################################################
      elif type(childNode) == list:
        childResults = []
        childContext = set(contextSet)
        # if your parent's nodeType is 'Assign' and you're in a list of
        # children then you're the LHS
        if nodeType == 'Assign':
          childContext.add('lhs')
        results = self.build_arg_list(childNode, childContext)
        parakeetNodeChildren.append(results)

      else:
        #######################################################################
        #  Literal
        #######################################################################
        if 'lhs' in contextSet:
          if not str(childNode) in self.arg_names:
            if nodeType == 'Name':
              raise ParakeetUnsupported(str(childNode) +
                                        " is a global variable")
        # assume last arg to build_parakeet_node for literals is a string
        parakeetNodeChildren.append(str(childNode))

    return build_parakeet_node(node, parakeetNodeChildren)

  def build_arg_list(self, python_nodes,  contextSet):
    results = []
    for node in python_nodes:
      funRef = self.register_if_function(node)
      if funRef is not None:
        funName = funRef.__module__ + "." + funRef.__name__
        print "registering", funName
        parName = LibPar.mk_var(c_char_p(funName),None)
        results.append(parName)
      else:
        result = self.visit(node, contextSet)
        if result is not None:
          results.append(result)
    return results

  def register_if_function(self, node):
    try:
      return self.get_function_ref(node)
    except RuntimeError:
      return None

  def get_function_ref(self, node):
    name = type(node).__name__
    if name == 'Name':
      funName = node.id
      if funName in self.global_variables:
        funRef = self.global_variables[funName]
      elif funName in __builtins__:
        funRef = __builtins__[funName]
      else:
        raise RuntimeError("[Parakeet] Couldn't find:" + funName)
    elif name == 'Attribute':
      #For function calls like mod1.mod2.mod4.fun()
      moduleList = []
      nextNode = node
      funName = node.attr
      #Get a list of the chain of modules

      while type(nextNode).__name__ == 'Attribute':
        moduleList = [nextNode.attr] + moduleList
        nextNode = nextNode.value

      currModule = self.global_variables[nextNode.id]

      for m in moduleList:
        currModule = currModule.__dict__[m]

      funRef = currModule
    else:
      raise RuntimeError("[Parakeet] Call.func shouldn't be", name)
    self.seen_functions.add(funRef)
    return funRef


  # everything except arrays

###############################################################################
#  Running function
###############################################################################

def register_function(f):
  info = function_info(f)
  if info is None: 
    raise RuntimerError("[Parakeet] Couldn't get info for function %s" % f)
  else:
    print "[parse_function] Parsing", info[1]
    arg_names = info[2]
    body_source = info[1]
    body_ast = ast.parse(body_source)
    AST = ASTConverter(f.func_globals, arg_names)
    parakeet_syntax = AST.visit(body_ast)
    #Med fix: right now, I assume there aren't any globals
    global_vars = []
    n_globals = len(global_vars)
    globals_array = list_to_ctypes_array(global_vars,c_char_p)

    arg_list = info[0]
    arg_array = list_to_ctypes_array(arg_list, c_char_p)
    n_args = len(arg_list)

    # register every function that was seen but not registered
    for other_fn in AST.seen_functions:
      if not VisitedFunctions.has_key(other_fn):
        #Possible fix:and not function_names?
        LOG("[register_function] Visiting %s" % other_fn.__name__)
        register_function(other_fn)
        LOG("[register_function] Visited %s" % other_fn.__name__)

    fun_name = f.__module__ + "." + f.__name__
    c_str = c_char_p(fun_name)
    fun_id = c_int( 
      LibPar.register_untyped_function(
        c_str, globals_array, n_globals, arg_array, n_args, parakeet_syntax))

    LOG("Registered %s as %s" % (f.__name__, fun_id))
    VisitedFunctions[f] = fun_id
    return fun_id 

# given a numpy array or a scalar, construct the equivalent parakeet value
def python_value_to_parakeet(arg):
  if isinstance(arg, np.ndarray):
    rank = len(arg.shape)
    inputShape = arg.ctypes.shape_as(c_int32)
    inputStrides = arg.ctypes.strides_as(c_int32)
    if rank > 1 and not arg.flags['C_CONTIGUOUS']:
      # until we have a proper interface for telling parakeet this data is
      # column-major, we have to manually transpose it
      # TODO: wouldn't strides be enough to handle this?
      arg = np.transpose(arg).copy()
    npType = arg.dtype.type
    if ((npType not in NumpyTypeToCtype) or
        (npType not in NumpyTypeToParakeetType)):
      raise Exception("Numpy element type unsupported: " + str(npType))
    ctype = NumpyTypeToCtype[npType]
    parakeetType = NumpyTypeToParakeetType[npType]
    dataPtr = arg.ctypes.data_as(POINTER(ctype))
    # TODO: This probably doesn't work for more than 2D - need recursion
    # TODO: mk_vec no longer exists
    #for z in range(len(arg.shape)):
    #  parakeetType = c_void_p(LibPar.mk_vec(parakeetType))
    parakeetVal = LibPar.mk_host_array(dataPtr, parakeetType, inputShape, rank,
                                       inputStrides, rank, arg.nbytes)
    return c_void_p(parakeetVal)
  elif np.isscalar(arg):
    if type(arg) == int:
      return LibPar.mk_int32(arg)
    elif type(arg) == float or type(arg) == np.float64:
      return LibPar.mk_float64(c_double(arg))
    elif type(arg) == np.float32:
      return LibPar.mk_float32(c_float(arg))
  else:
    raise Exception ("Input not supported by Parakeet: " + str(arg))

def array_from_memory(pointer, shape, dtype):
  from_memory = ctypes.pythonapi.PyBuffer_FromReadWriteMemory
  from_memory.restype = ctypes.py_object
  arr = np.empty(shape=shape,dtype=dtype)
  arr.data = from_memory(pointer,arr.nbytes)
  return arr

def parakeet_value_to_python(val):
  if val.is_scalar:
    c_type = ParakeetTypeToCtype[val.data.scalar.ret_type]
    result = 0
    if c_type == c_bool:
      result = val.data.scalar.ret_scalar_value.boolean
    elif c_type == c_int:
      result = val.data.scalar.ret_scalar_value.int32
    elif c_type == c_int64:
      result = val.data.scalar.ret_scalar_value.int64
    elif c_type == c_float:
      result = val.data.scalar.ret_scalar_value.float32
    elif c_type == c_double:
      result = val.data.scalar.ret_scalar_value.float64
    else:
      raise RuntimeError("Return type not supported by Parakeet: " % str(c_type))
    return result
  else:    
    rank = val.data.array.shape_len

    SHAPE_TYPE = c_int * rank
    c_shape = SHAPE_TYPE.from_address(val.data.array.shape)
    shape = list(c_shape)

    STRIDES_TYPE = c_int * val.data.array.strides_len
    c_strides = STRIDES_TYPE.from_address(val.data.array.strides)
    strides = list(c_strides)

    parakeet_elt_type = LibPar.get_array_element_type(val.data.array.ret_type)
    c_elt_type = ParakeetTypeToCtype[parakeet_elt_type]
    nelts = reduce(lambda x,y: x * y, shape)
    ARRAY_TYPE = c_elt_type * nelts
    result_array = ARRAY_TYPE.from_address(val.data.array.data)

    np_result = np.ctypeslib.as_array(result_array)
    np_result.shape = shape
    np_result.strides = strides
    return np_result

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
    sys.stdout.flush()
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
      print "Got %d results", ret.results_len
      if ret.results_len > 0:
        return parakeet_value_to_python(ret.results[0])
      else:
        return 

  def call_original(self, *args, **kwds):
    return self.old_function(*args, **kwds)

def PAR(func):
  return WrappedFunction(func)

