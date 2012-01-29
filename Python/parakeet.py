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
  LibPar.mk_scalar_op.restype = c_void_p
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

SafeFunctions = {abs:ast_prim('abs'),
                 np.all:ast_prim('allpairs'),
                 np.arange:ast_prim('range'),
#                 np.argmin:ast_prim('argmin'),
                 math.exp:ast_prim('exp'),
                 math.log:ast_prim('log'),
                 math.pow:ast_prim('pow'),
                 math.sqrt:ast_prim('sqrt'),
                 para_libs.map:ast_prim('map'),
                 para_libs.reduce:ast_prim('reduce')
                 }
BuiltinPrimitives = {'Add':ast_prim('+'),
                     'Sub':ast_prim('-'),
                     'Mult':ast_prim('*'),
                     'Div':ast_prim('/'),
                     'Mod':ast_prim('mod'),
                     'Pow':ast_prim('exp'),
                     'LShift':'Not yet implemented',
                     'RShift':'Not yet implemented',
                     'BitOr':'Not yet implemented',
                     'BitXor':'Not yet implemented',
                     'BitAnd':'Not yet implemented',
                     'FloorDiv':'Not yet implemented',
                     'Invert':'Not yet implemented (bit-wise inverse)',
                     'Not':ast_prim('not'),
                     'And':ast_prim('and'),
                     'Or':ast_prim('or'),
                     'UAdd':'Not yet implemented',
                     'USub':ast_prim('neg'),
                     'Eq':ast_prim('eq'),
                     'NotEq':ast_prim('neq'),
                     'Lt':ast_prim('<'),
                     'LtE':ast_prim('<='),
                     'Gt':ast_prim('>'),
                     'GtE':ast_prim('>='),
                     'Is':'Not yet implemented',
                     'IsNot':'Not yet implemented',
                     'In':'Not yet implemented',
                     'NotIn':'Not yet implemented',
                     'Index':ast_prim('index'),
                     'Slice':ast_prim('slice')}
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
def list_to_ctypes_array(inputList, t):
  print "[list_to_ctypes_array]", inputList
  numElements = len(inputList)
  listStructure = t * numElements # Description of a ctypes array
  l = listStructure()
  for i in range(numElements):
    l[i] = inputList[i]
  return l

def read_file(fileName):
  try:
    f = open(fileName,'r')
    outString = f.readlines()
    f.close()
  except IOError:
    print "Error opening %s" % fileName
    return ''
  return outString

def function_info(functionObj):
  try:
    codeInfo = functionObj.__code__
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


###############################################################################
#  Converter
###############################################################################
class ASTConverter():
  def __init__(self,funcGlobals):
    #Should really be a set?
    self.seen_functions = set([]) #curr_function
    self.varList = []
    self.functionGlobalVariables = funcGlobals

  # currContext is a set of strings telling us which nodes are parents
  # of the current one in the syntax tree
  def visit(self, node, contextSet):
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
        return self.build_parakeet_array(elts)
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
        kw_args = {'fixed': self.build_parakeet_array([]),
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
            kw_args[kw] = self.build_parakeet_array([val])
        args = fun_arg
        para_arr_args = self.build_parakeet_array(arr_args)
        args.append(para_arr_args)
        args.append(kw_args['fixed'])
        args.append(kw_args['axis'])
        return self.build_call(funRef, args)
      elif funRef == para_libs.reduce:
        fun_arg = self.build_arg_list([node.args[0]], childContext)
        arr_args = self.build_arg_list(node.args[1:], childContext)
        print arr_args
        kw_args = {'fixed': self.build_parakeet_array([]),
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
            kw_args[kw] = self.build_parakeet_array(val_args)
          else:
            val = self.visit(kw_arg.value, childContext)
            kw_args[kw] = self.build_parakeet_array([val])
        args = fun_arg
        para_arr_args = self.build_parakeet_array(arr_args)
        args.append(para_arr_args)
        args.append(kw_args['fixed'])
        args.append(kw_args['axis'])
        args.append(kw_args['default'])
        return self.build_call(funRef, args)
      else:
        funArgs = self.build_arg_list(node.args, childContext)
        return self.build_call(funRef, funArgs)
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
      return self.build_parakeet_node(node, args)
      #args[1]...[n+1] is what's inside the tuple, not the tuple itself
      #[args[0], args[1],....,args[n+1]]
      #Subscript(expr value, slice slice, expr_context ctx)
    elif nodeType == 'Return':
      #Might not be right for no return
      if node.value is None:
        return LibPar.mk_return(self.build_parakeet_array([]),0,None)
      elif type(node.value).__name__ == "Tuple":
        children = ast.iter_child_nodes(node.value)
        elts = self.build_arg_list(children, contextSet)
        ret_args = list_to_ctypes_array(elts, c_void_p)
        return LibPar.mk_return(ret_args,len(elts),None)
      else:
        ret = self.visit(node.value, contextSet)
        ret_args = list_to_ctypes_array([ret], c_void_p)
        return LibPar.mk_return(ret_args,1,None)


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
          if not str(childNode) in self.varList:
            if nodeType == 'Name':
              raise ParakeetUnsupported(str(childNode) +
                                        " is a global variable")
        # assume last arg to build_parakeet_node for literals is a string
        parakeetNodeChildren.append(str(childNode))

    return self.build_parakeet_node(node, parakeetNodeChildren)

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


  def build_parakeet_array(self, elts):
    c_array = list_to_ctypes_array(elts, c_void_p)
    return LibPar.mk_array(c_array, len(elts), None)

  def build_parakeet_block(self, stmts):
    c_array = list_to_ctypes_array(stmts, c_void_p)
    return LibPar.mk_block(c_array, len(stmts), None)

  def register_if_function(self, node):
    try:
      return self.get_function_ref(node)
    except RuntimeError:
      return None

  def get_function_ref(self, node):
    name = type(node).__name__
    if name == 'Name':
      funName = node.id
      if funName in self.functionGlobalVariables:
        funRef = self.functionGlobalVariables[funName]
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

      currModule = self.functionGlobalVariables[nextNode.id]

      for m in moduleList:
        currModule = currModule.__dict__[m]

      funRef = currModule
    else:
      raise RuntimeError("[Parakeet] Call.func shouldn't be", name)
    self.seen_functions.add(funRef)
    return funRef

  def build_call(self, funRef, args):
    if funRef in SafeFunctions:
      funNode = SafeFunctions[funRef]
    else:
      c_name = c_char_p(global_fn_name(funRef))
      funNode = LibPar.mk_var(c_name, None)
    funArgs = list_to_ctypes_array(args,c_void_p)
    return LibPar.mk_app(funNode, funArgs, len(funArgs), None)

  # everything except arrays
  def build_parakeet_node(self,node,args):
    #args is the children nodes in the correct type (i.e. node or literal)
    nodeType = type(node).__name__
    # either variable name or bool literal
    print "build_parakeet_node", node, args
    if nodeType == 'Name':
      #Special case for booleans
      if args[0] == 'True':
        LOG("bool(True)")
        return LibPar.mk_bool_paranode(1,None)
      elif args[0] == 'False':
        LOG("bool(False)")
        return LibPar.mk_bool_paranode(0,None)
      #Normal case
      else:
        LOG("var(%s)" % args[0])
        return LibPar.mk_var(c_char_p(args[0]),None)
    elif nodeType == 'Assign':
      print "Assign args", args
      LOG("assign(%s, %s)" % (args[0][0], args[1]))
      #Only support assigning a value to 1 variable at a time now
      return LibPar.mk_assign(args[0][0],args[1],0)
    #Mk a def where the target is the index node
    elif nodeType == 'BinOp':
      LOG("app(%s, [%s, %s])" % (type(node.op).__name__, args[0], args[2]))
      binArgs = list_to_ctypes_array([args[0],args[2]],c_void_p)
      operation = BuiltinPrimitives[type(node.op).__name__]
      return LibPar.mk_app(operation,binArgs,2,None)
    elif nodeType == 'UnaryOp':
      LOG("app(%s, [%s])" % (type(node.op).__name__, args[1]))
      unaryArg = list_to_ctypes_array([args[1]],c_void_p)
      operation = BuiltinPrimitives[type(node.op).__name__]
      return LibPar.mk_app(operation,unaryArg,1,None)
    elif nodeType == 'Compare':
      #Not sure when there are multiple ops or multiple comparators?
      LOG("app(%s, [%s, %s])" % \
          (type(node.ops[0]).__name__, args[0], args[2][0]))
      compArgs = list_to_ctypes_array([args[0],args[2][0]],c_void_p)
      operation = BuiltinPrimitives[type(node.ops[0]).__name__]
      return LibPar.mk_app(operation,compArgs,2,None)
    elif nodeType == 'Num':
      num = eval(args[0])
      LOG("%s(%s)" %(type(num), num))
      if type(num) == int:
        return LibPar.mk_int32_paranode(num,None)
      elif type(num) == float:
        return LibPar.mk_float_paranode(c_float(num),None)
    elif nodeType == 'Call':
      raise RuntimeError ('[Parakeet] Unexpected Call node')

    elif nodeType == 'Module':
      LOG("block(%s)" % str(args))
      return self.build_parakeet_block(args[0])
    elif nodeType == 'If':
      LOG("if(%s, %s, %s)" % (args[0], args[1], args[2]))
      thenBlock = self.build_parakeet_block(args[1])
      elseBlock = self.build_parakeet_block(args[2])
      return LibPar.mk_if(args[0], thenBlock, elseBlock, None)
    elif nodeType == 'While':
      LOG("while(%s, %s)" % (args[0], args[1]))
      block = self.build_parakeet_block(args[1])
      return LibPar.mk_whileloop(args[0],block,None)
    elif nodeType == 'Subscript':
      LOG("app(%s, %s, %s)" % (type(node.slice).__name__, args[0], args[1]))
      operation = BuiltinPrimitives[type(node.slice).__name__]
      arrayArgs = list_to_ctypes_array(args,c_void_p)
      return LibPar.mk_app(operation,arrayArgs,len(args),None)
    elif nodeType == 'Index':
      LOG("Index %s" % str(args))
      return args[0]
    elif nodeType == 'Attribute':
      LOG("Attribute %s " % str(args))
      #NOTE: doesn't make ANY sense right now
      return args[1]
    elif nodeType == 'Return':
      #NOTE: Deprecated
      LOG("Return %s" % str(args))
      if type(node.value).__name__ == 'Tuple':
        print "WHAT????"
        return LibPar.mk_return(args[0][0],args[0][1],None)
      else:
        print "ARGS",args
        print "INTO",self.build_parakeet_array([args[0]])
        return LibPar.mk_return(self.build_parakeet_array([args[0]]),1,None)
    else:
      print "[Parakeet]", nodeType, "with args", str(args), "not handled"
      return None

###############################################################################
#  Running function
###############################################################################

def fun_visit(func,new_f):
  funInfo = function_info(func)
  if funInfo:
    print "I'M PARSING",funInfo[1]
    node = ast.parse(funInfo[1])
    AST = ASTConverter(func.func_globals)
    AST.varList = funInfo[2]
    finalTree = AST.visit(node,set([]))
    #LOG(PrettyAST.printAst(node))
    #Med fix: right now, I assume there aren't any globals
    #Fix: functionGlobals[func] = globalVars
    global_vars = []
    global_vars_array = list_to_ctypes_array(global_vars,c_char_p)
    var_list = list_to_ctypes_array(funInfo[0],c_char_p)
    for key in AST.seen_functions:
      if not (VisitedFunctions.has_key(key)):
        #Possible fix:and not function_names?
        LOG("Visiting %s" % key.__name__)
        fun_visit(key,key)
        LOG("Visited %s" % key.__name__)
    fun_name = func.__module__ + "." + func.__name__
    c_str = c_char_p(fun_name)
    register = LibPar.register_untyped_function
    funID = c_int(register(c_str,
                           global_vars_array,
                           len(global_vars),
                           var_list,
                           len(funInfo[0]),
                           finalTree))
    LOG("Registered %s" % func.__name__)
    VisitedFunctions[func] = funID
    VisitedFunctions[new_f] = funID

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
    cEltType = ParakeetTypeToCtype[val.data.scalar.ret_type]
    result = 0
    if cEltType == c_bool:
      result = val.data.scalar.ret_scalar_value.boolean
    elif cEltType == c_int:
      result = val.data.scalar.ret_scalar_value.int32
    elif cEltType == c_int64:
      result = val.data.scalar.ret_scalar_value.int64
    elif cEltType == c_float:
      result = val.data.scalar.ret_scalar_value.float32
    elif cEltType == c_double:
      result = val.data.scalar.ret_scalar_value.float64
    else:
      raise Exception ("Return type not supported by Parakeet: " +
                       str(cEltType))
    return result
  else:
    nelts = 1
    rank = val.data.array.shape_len
    shape_type = c_int * rank
    cshape = shape_type.from_address(val.data.array.shape)
    shape = []
    for s in cshape:
      shape.append(s)
      nelts *= s
    strides_type = c_int * val.data.array.strides_len
    cstrides = strides_type.from_address(val.data.array.strides)
    strides = []
    for s in cstrides:
      strides.append(s)
    par_elt_type = LibPar.get_element_type(val.data.array.ret_type)
    cEltType = ParakeetTypeToCtype[par_elt_type]
    arrayType = cEltType * nelts
    resultArray = arrayType.from_address(val.data.array.data)
    npResult = np.ctypeslib.as_array(resultArray)
    npResult.shape = shape
    npResult.strides = strides
    return npResult

def run_function(func, args):
  sys.stdout.flush()
  numArgs = len(args)
  inputArr = []
  INPUTLIST = c_void_p*numArgs
  EMPTYLIST = c_int * 0
  ONELIST = c_int * 1
  inputs = INPUTLIST()
  for i in range(numArgs):
    inputs[i] = python_value_to_parakeet(args[i])
  # TODO: assume there are no globals now
  ret = LibPar.run_function(func, None, 0, inputs, numArgs)
  if (ret.return_code == 0):
    # TODO: again, we assume only one return val
    print "RETLEN", len(ret.results)
    return parakeet_value_to_python(ret.results[0])
  else:
    raise Exception("run_function failed")
  pass

def PAR(func):
  def new_f(*args, **kwds):
    return run_function(funID, args)
  new_f.__name__ = func.__name__
  new_f.__module__ = func.__module__
  if not (VisitedFunctions.has_key(new_f)):
    fun_visit(func, new_f)
  funID = VisitedFunctions[new_f]
  return new_f

