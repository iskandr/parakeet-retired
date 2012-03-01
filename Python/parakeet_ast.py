import ast, math, numpy as np, parakeet_lib
from ctypes import *
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
  len:parakeet_lib._len
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

ValidObjects = {np.add: parakeet_lib.add,
                np.subtract: parakeet_lib.sub,
                np.multiply: parakeet_lib.mult,
                np.divide: parakeet_lib.div}
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
  else:
    c_name = c_char_p(global_fn_name(python_fn))
    parakeet_fn = LibPar.mk_var(c_name, src_info.addr)
  return parakeet_fn

#Function to get ast node(s) for built-in functions/primitives
def ast_prim(sym):
  return c_void_p(LibPar.get_prim(sym))

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
        src_info = self.build_src_info(node)
        children = ast.iter_child_nodes(node)
        elts = self.build_arg_list(children, contextSet)
        return self.build_parakeet_array(src_info, elts)
      else:
        raise ParakeetUnsupported(
            "lists and tuples are not supported outside of numpy arrays")
    elif nodeType in ['Call', 'Subscript', 'Return', 'Assign','Attribute']:
      return self.build_complex_parakeet_node(node,contextSet)
    parakeetNodeChildren = []
    for childName, childNode in ast.iter_fields(node):
      #######################################################################
      #  Single AST Node
      #######################################################################
      if isinstance(childNode, ast.AST):
        # Don't need to visit the function node since
        # it will be handled directly by 'build_simple_parakeet_node'
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
        if not str(childNode) in self.arg_names:
          if nodeType == 'Name':
            if 'lhs' in contextSet:
              raise ParakeetUnsupported(str(childNode) +
                                        " is a global variable")
            else:
              self.global_variables.add(str(childNode))
        # assume last arg to build_simple_parakeet_node for literals is a string
        parakeetNodeChildren.append(str(childNode))

    return self.build_simple_parakeet_node(node, parakeetNodeChildren)

  def build_parakeet_array(self, src_info, elts):
    c_array = list_to_ctypes_array(elts, c_void_p)
    return LibPar.mk_array(c_array, len(elts), src_info.addr)

  def build_parakeet_block(self, src_info, stmts):
    c_array = list_to_ctypes_array(stmts, c_void_p)
    return LibPar.mk_block(c_array, len(stmts), src_info.addr)

  def build_call(self, src_info, python_fn, args):
    parakeet_fn = build_fn_node(src_info, python_fn)
    parakeet_args = list_to_ctypes_array(args,c_void_p)
    n = len(parakeet_args)
    LOG("Call(%s,%s,%s)" % (parakeet_fn, parakeet_args, n))
    return LibPar.mk_app(parakeet_fn, parakeet_args, n, src_info.addr)

  def build_prim_call(self, src_info, python_op_name, *args):
    LOG("build_prim_call(%s, %s)" % (python_op_name, args))
    parakeet_args = list_to_ctypes_array(args,c_void_p)
    parakeet_op_name = BuiltinPrimitives[python_op_name]
    if parakeet_op_name is None:
      raise RuntimeError('Prim not implemented: %s' % python_op_name)
    else:
      prim = ast_prim(parakeet_op_name)
    return LibPar.mk_app(prim, parakeet_args, len(parakeet_args),
                         src_info.addr)

  def build_var(self, src_info,name):
    #Special case for booleans
    if name == 'True':
      return LibPar.mk_bool_paranode(1, src_info.addr)
    elif name == 'False':
      return LibPar.mk_bool_paranode(0, src_info.addr)
    else:
      return LibPar.mk_var(c_char_p(name), src_info.addr)

  def build_num(self, src_info, str_num):
    """
    Given the string form of a number, build a syntax node for an int or float
    """
    num = eval(str_num)
    LOG("%s(%s)" %(type(num), num))
    if type(num) == int:
      return LibPar.mk_int32_paranode(num, src_info.addr)
    elif type(num) == float:
      return LibPar.mk_float_paranode(c_float(num), src_info.addr)
    else:
      assert False

  def build_simple_parakeet_node(self, node, args):
    """Build simple case syntax nodes which don't involve function arguments"""
    #args is the children nodes in the correct type (i.e. node or literal)
    node_type = name_of_ast_node(node)
    src_info = self.build_src_info(node)
    # either variable name or bool literal
    print "build_simple_parakeet_node", node, args
    if node_type == 'Name':
      return self.build_var(src_info, args[0])
    elif node_type == 'BinOp':
      return self.build_prim_call(src_info, name_of_ast_node(node.op), args[0],
                                  args[2])
    elif node_type == 'BoolOp':
      if len(args[1]) != 2:
        raise RuntimeError("[Parakeet] Unexpected number of args for:" +
                           node.op)
      return self.build_prim_call(src_info, name_of_ast_node(node.op),
                                  args[1][0], args[1][1])
    elif node_type == 'UnaryOp':
      return self.build_prim_call(src_info, name_of_ast_node(node.op), args[1])
    elif node_type == 'Compare':
      #Not sure when there are multiple ops or multiple comparators?
      return self.build_prim_call(src_info, name_of_ast_node(node.ops[0]),
                                  args[0], args[2][0])
    elif node_type == 'Subscript':
      return self.build_prim_call(src_info, name_of_ast_node(node.slice),
                                  args[0], args[1])
    elif node_type == 'Index':
      LOG("Index %s" % str(args))
      assert False
      #return args[0]
    elif node_type == 'Num':
      return self.build_num(src_info, args[0])
    elif node_type == 'Module':
      LOG("block(%s)" % str(args))
      return self.build_parakeet_block(src_info, args[0])
    elif node_type == 'If':
      LOG("if(%s, %s, %s)" % (args[0], args[1], args[2]))
      thenBlock = self.build_parakeet_block(src_info,args[1])
      elseBlock = self.build_parakeet_block(src_info,args[2])
      return LibPar.mk_if(args[0], thenBlock, elseBlock, src_info.addr)
    elif node_type == 'While':
      LOG("while(%s, %s)" % (args[0], args[1]))
      block = self.build_parakeet_block(src_info, args[1])
      return LibPar.mk_whileloop(args[0], block, src_info.addr)
    else:
      print "[Parakeet]", node_type, "with args", str(args), "not handled"
      return None

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

  def register_if_function(self, node):
    try:
      return self.get_function_ref(node)
    except RuntimeError:
      return None

  def get_function_ref(self, node):
    name = type(node).__name__
    if name == 'Name':
      funName = node.id
      if funName in self.global_refs:
        funRef = self.global_refs[funName]
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

      currModule = self.global_refs[nextNode.id]

      for m in moduleList:
        try:
          currModule = currModule.__dict__[m]
        except AttributeError:
          currModule = currModule.__getattribute__(m)
      funRef = currModule
    else:
      raise RuntimeError("[Parakeet] Call.func shouldn't be", name)
    if funRef in AutoTranslate:
      funRef = AutoTranslate[funRef]
    if not hasattr(funRef,'__self__') or not funRef.__self__:
      self.seen_functions.add(funRef)
    return funRef

  def build_complex_parakeet_node(self,node,contextSet):
    nodeType = type(node).__name__
    src_info = self.build_src_info(node)
    if nodeType == 'Call':
      funRef = self.get_function_ref(node.func)
      if hasattr(funRef,'__self__') and funRef.__self__:
        if funRef.__self__ in ValidObjects:
          func = ValidObjects[funRef.__self__]
          ### Should be it's own function?
          if func in AutoTranslate:
            func = AutoTranslate[func]
          self.seen_functions.add(func)
          fun_name = global_fn_name(func)
          print "registering", fun_name
          par_name = LibPar.mk_var(c_char_p(fun_name), src_info.addr)
          fun_arg = par_name
          ### End own function
          kw_args = {'fixed': self.build_parakeet_array(src_info,[])}
          if funRef.__name__ == "reduce" or "accumulate":
            arr_args = self.build_arg_list([node.args[0]], contextSet)
            kw_args['default'] = LibPar.mk_void(None)
            if len(node.args) == 1:
              kw_args['axis'] = LibPar.mk_int32_paranode(0, src_info.addr)
            elif len(node.args) == 2:
              kw_args['axis'] = self.visit(node.args[1], contextSet)
            else:
              print "TOO MANY ARGUMENTS FOR %s, DO NOT SUPPORT DTYPE" % (
                      funRef.__name__)
              assert False
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
                     'axis': LibPar.mk_void(None)
                    }
          if funRef == parakeet_lib.reduce:
            kw_args['default'] = LibPar.mk_void(None)
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
      return self.build_simple_parakeet_node(node, args)
      #args[1]...[n+1] is what's inside the tuple, not the tuple itself
      #[args[0], args[1],....,args[n+1]]

    elif nodeType == 'Return':
      #Might not be right for no return
      if node.value is None:
        LOG("return()")
        return LibPar.mk_return(None, 0, src_info.addr)
      elif type(node.value).__name__ == "Tuple":
        children = ast.iter_child_nodes(node.value)
        elts = self.build_arg_list(children, contextSet)
        ret_args = list_to_ctypes_array(elts, c_void_p)
        LOG("return(%s,%s)" % (ret_args, len(elts)))
        return LibPar.mk_return(ret_args, len(elts), src_info.addr)
      else:
        ret = self.visit(node.value, contextSet)
        ret_args = list_to_ctypes_array([ret], c_void_p)
        LOG("return(%s,%s)" % (ret_args, 1))
        return LibPar.mk_return(ret_args, 1, src_info.addr)
    elif nodeType == 'Assign':
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
      rhs_arg = self.visit(node.value, rightChildContext)
      LOG("assign(%s,%s,%s)" % (lhs_args, num_args, rhs_arg))
      return LibPar.mk_assign(lhs_args, num_args, rhs_arg, src_info.addr)
    elif nodeType == 'Attribute':
      if 'lhs' in contextSet:
        raise RuntimeError("[Parakeet] Assignment to attributes not supported")
      currNodeType = nodeType
      currNode = node
      var_str = ""
      while (currNodeType != "Name"):
        var_str = "." + currNode.attr + var_str
        currNode = currNode.value
        currNodeType = type(currNode).__name__
      var_str = currNode.id + var_str
      if var_str in self.arg_names:
        raise ("[Parakeet] Object parameters not supported")
      self.global_variables.add(var_str)
      c_name = c_char_p(var_str)
      return LibPar.mk_var(c_name, src_info.addr)
    else:
      raise RuntimerError("[Parakeet] %s not supported in build_complex_" +
                          "parakeet_node" % nodeType)

  def build_src_info(self, node):
    try:
      file_name = c_char_p(self.file_name)
      line = c_int(self.line_offset + node.lineno)
      col = c_int(node.col_offset)
      return _source_info_t(_c_source_info_t(file_name, line, col))
    except AttributeError:
      return _source_info_t(None)

###############################################################################
#  Function Registration
###############################################################################

def register_function(f):
  info = function_info(f)
  LOG("[Parakeet] Registering %s : %s" % (f, info))
  if info is None:
    raise RuntimerError("[Parakeet] Couldn't get info for function %s" % f)
  else:
    print "[parse_function] Parsing", info[1]
    file_name = f.__code__.co_filename
    line_offset = f.__code__.co_firstlineno
    global_refs = f.func_globals
    arg_names = info[2]
    body_source = info[1]
    body_ast = ast.parse(body_source)
    body_ast = ast.fix_missing_locations(body_ast)
    AST = ASTConverter(global_refs, arg_names, file_name, line_offset)
    parakeet_syntax = AST.visit(body_ast)
    print "\n\n\n\n"
    print "PRINTING OUT THE NODE"
    LibPar.print_ast_node(parakeet_syntax)
    print "\n\n\n\n"
    #Med fix: right now, I assume there aren't any globals
    global_vars = list(AST.global_variables)
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

    fun_name = global_fn_name(f)
    c_str = c_char_p(fun_name)
    fun_id = c_int(
      LibPar.register_untyped_function(
        c_str, globals_array, n_globals, arg_array, n_args, parakeet_syntax))

    LOG("Registered %s as %s" % (f.__name__, fun_id))
    VisitedFunctions[f] = fun_id
    return fun_id, global_vars
