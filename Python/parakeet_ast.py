import ast, math, numpy as np, parakeet_lib
from ctypes import *
import parakeet_common
from parakeet_common import LOG, list_to_ctypes_array

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

def build_parakeet_array(self,  elts, src_info = None):
  arr = list_to_ctypes_array(elts)
  return LibPar.mk_array(arr, len(elts), src_addr(src_info))

def build_parakeet_block(self, stmts, src_info = None):
  arr = list_to_ctypes_array(stmts)
  return LibPar.mk_block(arr, len(stmts), src_addr(src_info))


def build_call(self, python_fn, args, kw_args = {}, src_info = None):
  kw_names = []
  kw_values = []
  for k,v in kw_args.items():
    kw_names.append(k)
    kw_values.append(v)
  parakeet_fn = build_fn_node(src_info, python_fn)
  parakeet_positional_args = list_to_ctypes_array(args)
  parakeet_kw_names = list_ctypes_array(map(c_char_p, kw_names))
  return LibPar.mk_call(
    parakeet_fn, parakeet_positional_args, len(args), 
    parakeet_kw_names, parakeet_kw_values, len(kw_args), 
    src_addr(src_info))

def build_prim_call(python_op_name, args, src_info = None):
  parakeet_args = list_to_ctypes_array(args)
  parakeet_op_name = BuiltinPrimitives[python_op_name]
  if parakeet_op_name is None:
    raise RuntimeError('Prim not implemented: %s' % python_op_name)
  else:
    prim = ast_prim(parakeet_op_name)
  return LibPar.mk_call(prim, parakeet_args, len(args), 
                        None, None, 0, src_addr(src_info))

def build_var(name, src_info = None):
  #Special case for booleans
  src = src_addr(src_info)
  if name == 'True':
    return LibPar.mk_bool_paranode(1, src)
  elif name == 'False':
    return LibPar.mk_bool_paranode(0, src)
  else:
    return LibPar.mk_var(c_char_p(name), src)

def build_num(str_num, src_info = None):
  """
  Given the string form of a number, build a syntax node for an int or float
  """
  num = eval(str_num)
  src = src_addr(src_info)
  if type(num) == int:
    return LibPar.mk_int32_paranode(num, src)
  elif type(num) == float:
    return LibPar.mk_float_paranode(c_float(num), src)
  else:
    raise RuntimeError("Failed to create number from " + str_num)


#module Python version "$Revision$"
#{
#  mod = Module(stmt* body)
#      | Interactive(stmt* body)
#      | Expression(expr body)
#
#      -- not really an actual node but useful in Jython's typesystem.
#      | Suite(stmt* body)
#
#  stmt = FunctionDef(identifier name, arguments args, 
#                            stmt* body, expr* decorator_list)
#        | ClassDef(identifier name, expr* bases, stmt* body, expr* decorator_list)
#        | Return(expr? value)
#
#        | Delete(expr* targets)
#        | Assign(expr* targets, expr value)
#        | AugAssign(expr target, operator op, expr value)
#
#        -- not sure if bool is allowed, can always use int
#         | Print(expr? dest, expr* values, bool nl)
#
#        -- use 'orelse' because else is a keyword in target languages
#        | For(expr target, expr iter, stmt* body, stmt* orelse)
#        | While(expr test, stmt* body, stmt* orelse)
#        | If(expr test, stmt* body, stmt* orelse)
#        | With(expr context_expr, expr? optional_vars, stmt* body)
#
#        -- 'type' is a bad name
#        | Raise(expr? type, expr? inst, expr? tback)
#        | TryExcept(stmt* body, excepthandler* handlers, stmt* orelse)
#        | TryFinally(stmt* body, stmt* finalbody)
#        | Assert(expr test, expr? msg)
#
#        | Import(alias* names)
#        | ImportFrom(identifier? module, alias* names, int? level)
#
#        -- Doesn't capture requirement that locals must be
#        -- defined if globals is
#        -- still supports use as a function!
#        | Exec(expr body, expr? globals, expr? locals)
#
#        | Global(identifier* names)
#        | Expr(expr value)
#        | Pass | Break | Continue
#
#        -- XXX Jython will be different
#        -- col_offset is the byte offset in the utf8 string the parser uses
#        attributes (int lineno, int col_offset)
#
#        -- BoolOp() can use left & right?
#  expr = BoolOp(boolop op, expr* values)
#       | BinOp(expr left, operator op, expr right)
#       | UnaryOp(unaryop op, expr operand)
#       | Lambda(arguments args, expr body)
#       | IfExp(expr test, expr body, expr orelse)
#       | Dict(expr* keys, expr* values)
#       | Set(expr* elts)
#       | ListComp(expr elt, comprehension* generators)
#       | SetComp(expr elt, comprehension* generators)
#       | DictComp(expr key, expr value, comprehension* generators)
#       | GeneratorExp(expr elt, comprehension* generators)
#       -- the grammar constrains where yield expressions can occur
#       | Yield(expr? value)
#       -- need sequences for compare to distinguish between
#       -- x < 4 < 3 and (x < 4) < 3
#       | Compare(expr left, cmpop* ops, expr* comparators)
#       | Call(expr func, expr* args, keyword* keywords,
#       expr? starargs, expr? kwargs)
#       | Repr(expr value)
#       | Num(object n) -- a number as a PyObject.
#       | Str(string s) -- need to specify raw, unicode, etc?
#       -- other literals? bools?
#
#       -- the following expression can appear in assignment context
#       | Attribute(expr value, identifier attr, expr_context ctx)
#       | Subscript(expr value, slice slice, expr_context ctx)
#       | Name(identifier id, expr_context ctx)
#       | List(expr* elts, expr_context ctx) 
#       | Tuple(expr* elts, expr_context ctx)
#
#        -- col_offset is the byte offset in the utf8 string the parser uses
#        attributes (int lineno, int col_offset)
#
#  expr_context = Load | Store | Del | AugLoad | AugStore | Param
#
#  slice = Ellipsis | Slice(expr? lower, expr? upper, expr? step) 
#        | ExtSlice(slice* dims) 
#        | Index(expr value) 
#
#  boolop = And | Or 
#
#  operator = Add | Sub | Mult | Div | Mod | Pow | LShift 
#                 | RShift | BitOr | BitXor | BitAnd | FloorDiv
#
#  unaryop = Invert | Not | UAdd | USub
#
#  cmpop = Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn
#
#  comprehension = (expr target, expr iter, expr* ifs)
#
#  -- not sure what to call the first argument for raise and except
#  excepthandler = ExceptHandler(expr? type, expr? name, stmt* body)
#                  attributes (int lineno, int col_offset)
#
#  arguments = (expr* args, identifier? vararg, 
#         identifier? kwarg, expr* defaults)
#
#        -- keyword arguments supplied to call
#        keyword = (identifier arg, expr value)
#
#        -- import name with optional 'as' alias.
#        alias = (identifier name, identifier? asname)

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

  
  # when we parse Python source, we get a single function definition 
  # wrapped by a Module node 
  def visit_module(self, node):
    assert isinstance(node, ast.Module)
    assert len(node.body) == 1
    self.visit_function_def(node.body[0])
  
  # TODO: don't ignore function decorators!  
  def visit_function_def(self, node):
    assert isinstance(node, ast.FunctionDef)
    # we don't support variable argument functions
    assert node.args.vararg is None
    # we don't support dictionaries of keyword arguments  
    assert node.args.kwarg is None
    # assume the function arguments have been pulled out elsewhere and 
    # we just need the body 
    self.visit_stmt_sequence(node.body)
    
  
  def visit_stmt_sequence(self, stmts):
    return build_parakeet_block([self.visit_stmt(stmt) for stmt in stmts])

  def visit_stmt(self, node):
    if isinstance(node, ast.If):
    
      #LOG("if(%s, %s, %s)" % (args[0], args[1], args[2]))
      thenBlock = self.build_parakeet_block(src_info,args[1])
      elseBlock = self.build_parakeet_block(src_info,args[2])
      return LibPar.mk_if(args[0], thenBlock, elseBlock, src_info.addr)
    elif isinstance(node, ast.Assign):
      self.visit_assign(node)
    elif isinstance(node, ast.Return):
    elif isinstance(node, ast.While):
      
    elif isinstance(node, ast.Expr):
      self.visit_expr(node.value)
    else:
      raise RuntimeError("Unsupported statement" + str(node))
        
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
  
  def visit_array_elts(self, node):
    if isinstance(node, ast.List) or isinstance(node, ast.Tuple):
      elts = [self.visit_expr(elt) for elt in node.elts]
      return build_parakeet_array(elts, self.build_src_info(node)) 
    else:
      raise ParakeetUnsupported('Array must have literal arguments') 
  # currContext is a set of strings telling us which nodes are parents
  # of the current one in the syntax tree
  def visit(self, node, contextSet=set([])):
    if 'lhs' in contextSet and isinstance(node, ast.Attribute):
      raise ParakeetUnsupported("changing a field is not allowed")
    if 'rhs' in contextSet and isinstance(node, ast.Str):
      raise ParakeetUnsupported("strings are not supported")
    
      else:
        raise ParakeetUnsupported(
            "lists and tuples are not supported outside of numpy arrays")
    elif type(node) in [ast.Call, ast.Subscript, ast.Return, ast.Assign, ast.Attribute]:
      return self.build_complex_parakeet_node(node,contextSet)
    parakeetNodeChildren = []
    for childName, childNode in ast.iter_fields(node):
      #######################################################################
      #  Single AST Node
      #######################################################################
      if isinstance(childNode, ast.AST):
        # Don't need to visit the function node since
        # it will be handled directly by 'build_simple_parakeet_node'
        if isinstance(node, ast.Call) and childName == 'func': continue
        childContext = set(contextSet)
        # if you're a single child whose parent's nodeType is assign
        # then you're the RHS
        if isinstance(node, ast.Assign):
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
        if isinstance(node, ast.Assign):
          childContext.add('lhs')
        results = self.build_arg_list(childNode, childContext)
        parakeetNodeChildren.append(results)

      else:
        #######################################################################
        #  Literal
        #######################################################################
        if not str(childNode) in self.arg_names:
          if isinstance(node, ast.Name):
            if 'lhs' in contextSet:
              raise ParakeetUnsupported(str(childNode) +
                                        " is a global variable")
            elif not str(childNode) in BuiltinStrs:
              self.global_variables.add(str(childNode))
        # assume last arg to build_simple_parakeet_node for literals is a string
        parakeetNodeChildren.append(str(childNode))

    return self.build_simple_parakeet_node(node, parakeetNodeChildren)


  def build_simple_parakeet_node(self, node, args):
    """Build simple case syntax nodes which don't involve function arguments"""
    #args is the children nodes in the correct type (i.e. node or literal)
    src_info = self.build_src_info(node)
    # either variable name or bool literal
    #print "build_simple_parakeet_node", node, args
    if isinstance(node, ast.Name):
      return self.build_var(src_info, args[0])
    elif isinstance(node, ast.BinOp):
      return self.build_prim_call(src_info, name_of_ast_node(node.op), args[0],
                                  args[2])
    elif isinstance(node, ast.BoolOp):
      if len(args[1]) != 2:
        raise RuntimeError("[Parakeet] Unexpected number of args for:" +
                           node.op)
      return self.build_prim_call(src_info, name_of_ast_node(node.op),
                                  args[1][0], args[1][1])
    elif isinstance(node, ast.UnaryOp):
      return self.build_prim_call(src_info, name_of_ast_node(node.op), args[1])
    elif isinstance(node, ast.Compare):
      #Not sure when there are multiple ops or multiple comparators?
      return self.build_prim_call(src_info, name_of_ast_node(node.ops[0]),
                                  args[0], args[2][0])
    elif isinstance(node, ast.Subscript):
      return self.build_prim_call(src_info, name_of_ast_node(node.slice), *args)
    elif isinstance(node, ast.Index):
      raise RuntimeError("[Parakeet] Unexpected index node in AST")
    elif isinstance(node, ast.Num):
      return self.build_num(src_info, args[0])
    
    
    elif isinstance(node, ast.While):
      #LOG("while(%s, %s)" % (args[0], args[1]))
      block = self.build_parakeet_block(src_info, args[1])
      return LibPar.mk_whileloop(args[0], block, src_info.addr)
    else:
      raise RuntimeError("[Parakeet] %s not supported " % node)
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

  def build_complex_parakeet_node(self, node, contextSet):
    src_info = self.build_src_info(node)
    if isinstance(node, ast.Call):
          elif isinstance(node, ast.Subscript):
      args = []
      parakeetArray = self.visit(node.value, contextSet)
      args.append(parakeetArray)
      #print "Node %s, fields %s" % (node, node._fields)
      #print "Node.value %s, fields %s" %(node.value, node.value._fields)
      #print "Node.slice %s, fields %s" %(node.slice,  node.slice._fields)
      #print "Node.slice.value %s,  fields %s" % (node.slice.value, node.slice.value._fields)
      if isinstance(node.slice, ast.Index):
        if isinstance(node.slice.value, ast.Tuple):
          for idx in node.slice.value.elts:
            parakeetIdx = self.visit(idx, contextSet)
            print idx, parakeetIdx
            args.append(parakeetIdx)
        else:
          args.append(self.visit(node.slice.value, contextSet))
      else:
        raise ParakeetUnsupported(
            "slicing of type %s is not supported: %s" % str(type(node.slice)))
      return self.build_simple_parakeet_node(node, args)

    elif isinstance(node, ast.Return):
      #Might not be right for no return
      if node.value is None:
        #LOG("return()")
        return LibPar.mk_return(None, 0, src_info.addr)
      elif isinstance(node.value, ast.Tuple):
        children = ast.iter_child_nodes(node.value)
        elts = self.build_arg_list(children, contextSet)
        ret_args = list_to_ctypes_array(elts, c_void_p)
        return LibPar.mk_return(ret_args, len(elts), src_info.addr)
      else:
        ret = self.visit(node.value, contextSet)
        ret_args = list_to_ctypes_array([ret], c_void_p)
        return LibPar.mk_return(ret_args, 1, src_info.addr)
    elif isinstance(node, ast.Assign):
      leftChildContext = set(contextSet)
      rightChildContext = set(contextSet)
      leftChildContext.add('lhs')
      rightChildContext.add('rhs')
      if len(node.targets) != 1:
        #This shouldn't happen
        assert False
      if isinstance(node.targets[0], ast.Tuple):
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
    elif isinstance(node, ast.Attribute):
      if 'lhs' in contextSet:
        raise RuntimeError("[Parakeet] Assignment to attributes not supported")
      var_str = self.get_global_var_name(node)
      var_parts = var_str.split('.')
      if var_parts[0] in self.arg_names:
        method = '.'.join(var_parts[1:])
        if method in NumpyArrayAttributes:
          var_node = self.build_var(src_info, var_parts[0])
          return self.build_call(src_info, method, [var_node])
        else:
          raise ("[Parakeet] Unsupported local attribute %s" % (
                   '.'.join(var_parts[1:])))
      else:
        self.global_variables.add(var_str)
        c_name = c_char_p(var_str)
        return LibPar.mk_var(c_name, src_info.addr)
    else:
      raise RuntimerError("[Parakeet] %s not supported in build_complex_" +
                          "parakeet_node" % str(type(node)))

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

  for other_fn in AST.seen_functions:
    if not VisitedFunctions.has_key(other_fn):
      print 
      print "...found other_fn", other_fn
      print  
      register_function(other_fn)

  global_vars = list(AST.global_variables)
  n_globals = len(global_vars)
  globals_array = list_to_ctypes_array(global_vars,c_char_p)


  n_defaults = len(argspec.defaults)
  positional = argspec.args[:-n_defaults]
  n_positional = len(positional)
  positional_args_array = list_to_ctypes_array(positional, c_char_p)
  default_args = arg_names[n_positional:]
  default_args_array = list_to_ctypes_array(default_args, c_char_p)
  parakeet_default_values = \
    [python_value_to_parakeet(v) for v in argspec.defaults]
  default_values_array = list_to_ctypes_array(parakeet_default_values)
  
  # register every function that was seen but not registered

  fn_name_c_str = c_char_p(global_fn_name(f))
  fun_id = c_int(
    LibPar.register_untyped_function(
      fn_name_c_str, 
      globals_array, n_globals, 
      postional_args_array, n_positional,
      default_args_array, default_values_array, n_defaults, 
      parakeet_syntax))

  VisitedFunctions[f] = fun_id
  return fun_id, global_vars
