from ctypes import *
import ast, os, sys
import functools as ft
import numpy as np
import PrettyAST

###############################################################################
#  Initializations
###############################################################################
Verbose = 1
Debug = 0

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
def return_type_init():
  LibPar.parakeet_init()
  #NOTE: can set default to c_void_p, much less initialization?                
  LibPar.mk_def.restype = c_void_p
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
return_type_init()

###############################################################################
#  Global variables
###############################################################################

SafeFunctions = {np.all:ast_prim('allpairs'),
                 np.arange:ast_prim('range'),
#                 np.argmin:ast_prim('argmin'),
                 map:ast_prim('map')}
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
                     'Uadd':'Not yet implemented',
                     'Usub':ast_prim('neg'),
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
                     'Index':ast_prim('index')}
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

###############################################################################
#  Converter
###############################################################################
class ASTConverter():
  def __init__(self,funcGlobals):
    #Should really be a set?
    self.functionCalls = {} #curr_function
    self.varList = []
    self.evilFunction = ""
    self.functionGlobalVariables = funcGlobals

  def visit(self,node,currContext,currRightAssignment):
    nodeType = type(node).__name__
    if nodeType == 'Print':
      self.evilFunction == "printing is not allowed"
    if currContext == 'assignment':
      if nodeType == 'Attribute':
        self.evilFunction = "changing a field is not allowed"
    if currRightAssignment:
      if nodeType == 'Str':
        self.evilFunction = "strings are not supported"
      elif nodeType == 'List' or nodeType == 'Tuple':
        self.evilFunction = \
          "lists and tuples are not supported outside of numpy arrays"
    if self.evilFunction:
      return
    parakeetNodeChildren = []
    for fieldName,childNode in ast.iter_fields(node):
      if isinstance(childNode,ast.AST):
        #######################################################################
        #  Single AST Node
        #######################################################################
        if nodeType == 'Call' and fieldName == 'func':
          continue 
        #The only non-list child of Assign is the RHS
        if nodeType == 'Assign':
          nextRightAssignment = 1
        else:
          nextRightAssignment = currRightAssignment

        parakeetNodeChildren.append(self.visit(childNode,currContext,
                                               currRightAssignment))

        #######################################################################
        #  List of AST Nodes
        #######################################################################
      elif type(childNode) == list:# and isinstance(childNode[0],ast.AST):
        listParakeetNodeChildren = []
        for child in childNode:
          if nodeType == 'Assign':
            nextContext = 'assignment'
          else:
            nextContext = currContext
          listParakeetNodeChildren.append(self.visit(child,nextContext,
                                                     currRightAssignment))
          currContext = ''
        parakeetNodeChildren += [listParakeetNodeChildren]
      else:
        #######################################################################
        #  Literal
        #######################################################################
        if currContext == 'assignment':
          if not str(childNode) in self.varList:
            if nodeType == 'Name':
              self.evilFunction = str(childNode) + " is a global variable"

        parakeetNodeChildren.append(str(childNode))

    return self.paranodes(node, parakeetNodeChildren)

  def paranodes(self,node,args):
    #args is the children nodes in the correct type (i.e. node or literal) 
    nodeType = type(node).__name__
    verbString = 'Python Note: '+nodeType+" has no verbString"
    if Verbose and Debug:
      print "HANDLING",nodeType,args
    if nodeType == 'Name':
      #Special case for booleans
      if args[0] == 'True':
        verbString = "bool(True)"
        retNode = c_void_p(LibPar.mk_bool_paranode(1,None))
      elif args[0] == 'False':
        verbString = "bool(False)"
        retNode = c_void_p(LibPar.mk_bool_paranode(0,None))
      #Normal case
      else:
        verbString = "var(" + str(args[0]) + ")"
        retNode = c_void_p(LibPar.mk_var(c_char_p(args[0]),None))
    elif nodeType == 'Assign':
      verbString = "def(" +str(node.targets[0].id) + "," + str(args[1]) + ")"
      #Only support assigning a value to 1 variable at a time now
      retNode = c_void_p(LibPar.mk_def(c_char_p(node.targets[0].id),args[1],0))
    elif nodeType == 'BinOp':
      verbString = "app(" + type(node.op).__name__ + ",[" + str(args[0]) + \
                   "," + str(args[2]) + "])"
      binArgs = list_to_ctypes_array([args[0],args[2]],c_void_p)
      operation = BuiltinPrimitives[type(node.op).__name__]
      retNode = c_void_p(LibPar.mk_app(operation,binArgs,2,None))
    elif nodeType == 'UnaryOp':
      verbString = "app("+type(node.op).__name__+",["+str(args[1])+"])"
      unaryArg = list_to_ctypes_array([args[1]],c_void_p)
      operation = BuiltinPrimitives[type(node.op).__name__]
      retNode = c_void_p(LibPar.mk_app(operation,unaryArg,1,None))
    elif nodeType == 'Compare':
      #Not sure when there are multiple ops or multiple comparators?
      
      verbString = "app("+type(node.ops[0]).__name__+",["+str(args[0]) \
                    +","+str(args[2][0])+"])"
      compArgs = list_to_ctypes_array([args[0],args[2][0]],c_void_p)
      operation = BuiltinPrimitives[type(node.ops[0]).__name__]
      retNode = c_void_p(LibPar.mk_app(operation,compArgs,2,None))
    elif nodeType == 'Num':
      num = eval(args[0])
      if type(num) == int:
        verbString = "int32("+args[0]+")"
        retNode = c_void_p(LibPar.mk_int32_paranode(num,None))
      elif type(num) == float:
        verbString = "float32("+args[0]+")"
        retNode = c_void_p(LibPar.mk_float_paranode(c_float(num),None))
    elif nodeType == 'Call':
      #Simple function call
      if type(node.func).__name__ == 'Name':
        funName = node.func.id
        verbString = "app("+str(funName)+","+str(args[0])+")"
        #Special case for partial
        if funName == 'partial':
          funName = node.args[0].id
          args[1] = args[1][1:]
          verbString = "PARTIAL("+verbString+")"
        try:
          try:
            funRef = self.functionGlobalVariables[funName]
          except:
            #It could still be a built-in
            funRef = __builtins__[funName]
        except:
          print "Python Error: Couldn't find:",funName
        self.functionCalls[funRef] = 1
        try:
          funNode = SafeFunctions[funRef]
        except:
          try:
            funNode = c_void_p(LibPar.mk_var(c_char_p(funRef.__module__+"."+
                                                      funRef.__name__),None))
          except:
            print "Temp: we need this for calling",funName
            funNode = c_void_p(LibPar.mk_var(c_char_p(funRef.__module__+"."+
                                                      funName),None))
        funArgs = list_to_ctypes_array(args[1],c_void_p)
        numArgs = len(args[1])
      elif type(node.func).__name__ == 'Attribute':
        #For function calls like mod1.mod2.mod4.fun()
        moduleList = []
        nextNode = node.func
        funName = node.func.attr
        #Get a list of the chain of modules
        while type(nextNode).__name__ == 'Attribute':
          nextNode = nextNode.value
          moduleList = [nextNode.attr] + moduleList
        nextModu = self.functionGlobalVariables[nextNode.value.id]
        for modu in moduleList:
          nextModu = nextModu.__dict__[modu]
        funRef = nextModu.__dict__[funName]
        self.functionCalls[funRef] = 1
        try:
          funNode = SafeFunctions[funRef]
        except:
          try:
            funNode = c_void_p(LibPar.mk_var(c_char_p(funRef.__module__+"."+
                                                      funRef.__name__),None))
          except:
            print "Temp: we need this for calling",funName
            funNode = c_void_p(LibPar.mk_var(c_char_p(funRef.__module__+"."+
                                                      funName),None))
        #Bug fix: Arguments get stored in args[2] instead
        funArgs = list_to_ctypes_array(args[2],c_void_p)
        numArgs = len(arsg[2])
      else:
        print "Python Error: Call.func shouldn't be",type(node.func).__name__
      verbString = "app("+str(funNode)+","+str(funArgs)+","+str(numArgs)+")"
      retNode = c_void_p(LibPar.mk_app(funNode,funArgs,numArgs,None))
    elif nodeType == 'Module':
      verbString = "block("+str(args[0])+")"
      numArgs = len(args[0])
      block = list_to_ctypes_array(args[0],c_void_p)
      retNode = c_void_p(LibPar.mk_block(block,numArgs,None))
    elif nodeType == 'If':
      print args
      verbString = "if("+str(args[0])+",thenbb("+str(args[1])+"), elsebb(" + \
                   str(args[2]) + "))"
      cThenBB = list_to_ctypes_array(args[1], c_void_p)
      cElseBB = list_to_ctypes_array(args[2], c_void_p)
      thenAddr = LibPar.mk_block(cThenBB, len(args[1]), None)
      thenBB = c_void_p(thenAddr)
      elseBB = c_void_p(LibPar.mk_block(cElseBB, len(args[2]), None))
      retAddr = LibPar.mk_if(args[0], thenBB, elseBB, None)
      retNode = c_void_p(retAddr)
      print "\n thenBB: %u, %u, %s\n" % (thenAddr, thenBB.value, str(thenBB))
      print "\nRetnode: %u, %u, %s\n" %(retAddr, retNode.value, str(retNode))

    elif nodeType == 'While':
      verbString = "while("+str(args[0])+",block("+ str(args[1])+ "))"
      numStmt = len(args[1])
      blockArgs = list_to_ctypes_array(args[1],c_void_p)
      block = c_void_p(LibPar.mk_block(blockArgs,numStmt,None))
      retNode = c_void_p(LibPar.mk_whileloop(args[0],block,None))
    elif nodeType == 'Subscript':
      verbString = "app("+type(node.slice).__name__+"["+str(args[0])+","+\
                   str(args[1])+"])"
      operation = BuiltinPrimitives[type(node.slice).__name__]
      arrayArgs = list_to_ctypes_array([args[0],args[1]],c_void_p)
      retNode = c_void_p(LibPar.mk_app(operation,arrayArgs,2,None))
    elif nodeType == 'Index':
      print args
      retNode = args[0]
    elif nodeType == 'Attribute':
      retNode = args[1]
    elif nodeType == 'Return':
      return args[0]
    else:
      print "Python note:",nodeType,"not handled specially"
      return nodeType
    if Verbose:
      print verbString
    return retNode

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
    finalTree = AST.visit(node,0,0)
    if AST.evilFunction:
      print func.__name__,"is evil because:",AST.evilFunction
      return
    else:
      if Verbose:
        PrettyAST.printAst(node)
      #Med fix: right now, I assume there aren't any globals
        #Fix: functionGlobals[func] = globalVars      
      globList = list_to_ctypes_array([],c_char_p)

      varList = list_to_ctypes_array(funInfo[0],c_char_p)

      for key in AST.functionCalls.keys():
        if not (VisitedFunctions.has_key(key)):
          #Possible fix:and not function_names?
          if Verbose:
            print "Visiting",key.__name__#,VisitedFunctions,key
          fun_visit(key,key)
          if Verbose:
            print "Visited",key.__name__

      funID = c_int(LibPar.register_untyped_function(c_char_p(func.__module__+
                                                    "."+func.__name__),
                                                    globList,
                                                    0, #len(globalVars)
                                                    varList,
                                                    len(funInfo[0]),
                                                    finalTree))

      if Verbose:
        print "Registered",func.__name__
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
      return c_void_p(LibPar.mk_int32(arg))
    elif type(arg) == float or type(arg) == np.float64:
      return c_void_p(LibPar.mk_float64(c_double(arg)))
    elif type(arg) == np.float32:
      return c_void_p(LibPar.mk_float32(c_float(arg)))
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
