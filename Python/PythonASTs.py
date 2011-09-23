import numpy as np
import functools as ft
import ast
from ctypes import *
import PrettyAST

testing = 0
debug = 0
printing = 0
simpleTest = 0
import os
libtest = 0 #NOTE: JUST TO BE GLOBAL
#Platform dependent?
#libc = cdll.LoadLibrary("libc.so.6")

####STRUCT
class _U(Union):
  _fields_ = [("error_msg",c_char_p),
              ("results",POINTER(c_void_p))]

class return_val_t(Structure):
  _fields_ = [("return_code",c_int),
              ("results_len",c_int),
              ("ret_types",POINTER(c_void_p)),
              ("shapes",POINTER(POINTER(c_int))),
              ("data",_U)]
  
#INITIALIZATION
def returnTypeInit():
  #NOTE: can set default to c_void_p, much less initialization?
  libtest.mk_def.restype = c_void_p
  libtest.mk_int32_paranode.restype = c_void_p
  libtest.mk_int64_paranode.restype = c_void_p
  libtest.mk_var.restype = c_void_p
  libtest.mk_scalar_op.restype = c_void_p
  libtest.mk_app.restype = c_void_p
  libtest.mk_lam.restype = c_void_p
  libtest.mk_block.restype = c_void_p
  libtest.mk_scalar.restype = c_void_p
  libtest.mk_vec.restype = c_void_p
  libtest.mk_host_array.restype = c_void_p
  libtest.register_untyped_function.restype = c_int
  libtest.run_function.restype = return_val_t
  libtest.mk_int32.restype = c_void_p
  libtest.mk_float32.restype = c_void_p
  libtest.mk_float64.restype = c_void_p
  libtest.mk_whileloop.restype = c_void_p
  libtest.mk_bool.restype = c_void_p
  libtest.get_prim.restype = c_void_p
  libtest.mk_float_paranode.restype = c_void_p
  libtest.mk_double_paranode.restype = c_void_p

def ast_prim(sym):
  return c_void_p(libtest.get_prim(sym))

libcude = cdll.LoadLibrary('/usr/local/cuda/lib/libcudart.so.3')
libtest = cdll.LoadLibrary(os.getcwd() + '/../_build/libparakeetpy.so')
libtest.parakeet_init()
returnTypeInit()
###########

safe_functions = {np.all:ast_prim('all'),
#                  np.argmin:ast_prim('argmin'),
                  map:ast_prim('map'),
                  np.mean:ast_prim('mean'),
                  np.sum:ast_prim('sum')}
builtin_primitives = {'Add':ast_prim('+'),
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
function_asts = safe_functions.copy()
function_globals = {}
#List of which built-in functions are allowed and which aren't
#Old code:
primitives = {"abs":1,"all":1,"any":1,"basestring":1,"bin":0,"bool":0,
              "bytearray":1,
              "callable":1,"chr":0,"classmethod":0,"cmp":1,"compile":0,
              "complex":0,
              "delattr":0,"dict":0,"dir":0,"divmod":1, "enumerate":0, "eval":0,
              "execfile":0,"file":0,"filter":0,"float":1,"format":0,
               "frozenset":0,
              "getattr":1,"globals":1,"hasattr":1,"hash":1,"help":0, "hex":0,
              "id":1,
              "input":0,"int":1,"isinstance":1, "issubcalss": 1, "iter":0,
              "len":1,
              "list":0, "locals":1, "long":1, "map": 1, "max":1,
              "memoryview":0,
              "min":1,"next":1,"object":0,"oct":0,"open":0,"ord":0,"pow":1,
              "print":0,
              "property":1,"range":0,"raw_input":0,"reduce":1,"reload":1,
              "repr":0,
              "reversed":0,"round":1,"set":0,"settattr":0,"slice":0,"sorted":0,
              "staticmethod":0,"str":0,"sum":1,"super":0,"tuple":0,"type":1,
              "unichr":0,
              "unicode":0,"vars":1,"xrange":0,"zip":0}


numpyPrimitives = {np.all:1,np.sum:1,np.argmin:1,np.mean:1}
functoolsPrimitives = {ft.partial:1}
newPrimitves = {"sum":"sum","argmin":"argmin","map":"map","mean":"mean",
                "arange":"range","all":"all"}

def list_to_ctypes_array(input_list,type):
  num_elements = len(input_list)
  list_structure = type * num_elements
  list = list_structure()
  for i in range(num_elements):
    list[i] = input_list[i]
  return list

class ASTPrinter(ast.NodeVisitor):
  def __init__(self):
    ast.NodeVisitor.__init__(self)
  def newRow(self):
    pass
  def printLiteral(self,lit):
    pass
  def printNode(self,lit):
    pass
  def prettyPrint(self):
    print "Not printing"

if printing == 1:
  from ASTPrint import ASTPrinter

#Note: keep context as a hash table, or just make in_assignment a string? 
class ASTCreator(ASTPrinter):
  func_handled = {} #Note; deprecated, shouldn't use anymore
  def __init__(self,file_name):
    self.file_name = file_name
    self.npArrayEvil = ""
    self.context = {'assignment':1,'function_call':2,'attribute':3}
    self.right_assignment = 0
    self.npArrayEvil = ""
    self.in_bad = 0
    #Keeps track of current functions
    self.curr_function = {}
    #Keeps track of how to reference function
    self.function_path = file_name
    self.curr_path = ''
    #Keeps track of variables
    self.var_list = []
    #Keeps track of if/why it's a bad function
    self.evil_function = ""
    #Keeps track of nicknamed imports
    self.alias_import = {}
    self.from_import = {}
    #Do normal init stuff
    ASTPrinter.__init__(self)
    self.func_name = ""
  #Visiting a node
  def getFunction(self,node):
#    if str(node) in self.var_list:
#      self.evil_function = "calling a variable as a function is not supported"
#      print str(node)    
    func_str = self.function_path + '.' + str(node)
    try:
      exec "curr_func = %s" % func_str
      print "Not in except block"
    except:
      try:
        exec "curr_func = np.%s" %str(node)
      except:
        try:
          import addn
          exec "curr_func = %s.%s"%(self.file_name,str(node))
        except:
          print "Failed trying to find the function",str(node)
#      try:
#        exec "curr_func = %s.__globals__['%s']" % (self.func_name , str(node))
#        func_str = str(node)
#      except:
#        str_node = str(node)
#        try:
#          exec "import %s" % self.from_import[str_node]
#          str_node = self.from_import[str_node] + "." + str_node
#        except:
#          pass
#        exec "curr_func = %s" % str_new_node
#        try:
#          if not primitives[str(node)]:
#            self.evil_function = "%s is an unsupported primitive" % str(node)
#        except:
#          pass #Note: Not a primitive, use this somehow?
    return curr_func
  
  def from_imports(self,node):
    node_type = type(node).__name__
    if node_type == 'ImportFrom':
      try:
        for al in node.names:
          self.from_import[al.name] = node.module
      except:
        print 'error with from import code'
    if type(node).__name__ == 'alias':
      try:
        self.var_list.remove(node.asname)
        self.alias_import[node.asname] = node.name
      except:
        self.var_list.remove(node.name)

  #This is incorrect like getFunction
  def handling_attributes(self):
    func_str = self.function_path + self.curr_path
    try:
      #Try just getting the reference directly
      exec "curr_func = %s" % func_str
    except:
      #Otherwise there was some from x import y involved, so
      #import the module that the first in a chain of from x 
      #import y is in (which is the last module referenced),
      #and then use globals to find the actual reference to the
      #function
      module = func_str.split('.')
      module.reverse()
      module = module[1]
      try:
        exec "import %s" % module
      except:
        exec "import %s as %s" % (self.alias_import[module],module)
      func_str = module + '.' + func_str.split('.').pop()
      exec "curr_func = %s" % (func_str)
    #This is to catch when numpy arrays are made
    if type(curr_func).__name__ == 'classobj' or \
    type(curr_func).__name__ == 'builtin_function_or_method':
      if (self.right_assignment):
        self.curr_function[curr_func] = 1
        if (func_str.split('.').pop() == 'array'):
          self.npArrayEvil = "Safe"
        #If it's not a numpy array, then it's not handled by parakeet
        else:
          self.evil_function = "a complicated object was created"
    else:
      #Otherwise, we're gonna say it's a function
      self.curr_function[curr_func] = 1
    self.curr_path = ''

           
  def visit(self,node,in_assignment):
    #Some tests for evilness
    node_type = type(node).__name__
    if node_type == "Print":
      self.evil_function = "printing is not allowed"
    if in_assignment == self.context["assignment"]:
      if node_type == "Attribute":
        self.evil_function = "changing a field is not allowed"
    if self.right_assignment:
      if node_type == "Str":
        self.evil_function = "strings are not supported"
      if node_type == "List" or node_type == "Tuple":
        #Special exception for numpy arrays
        if self.npArrayEvil != "Safe":
          self.npArrayEvil = "lists and tuples are not supported outside",
          "of numpy arrays"
        #Note: do something here to check for global variables?
      if self.evil_function:
        #Means that the function's been found to be evil for some reason,
        #don't try and build the AST
        return
    args = []
    #Visits all of the child nodes
    for field_name,child_node in ast.iter_fields(node):
#------------------------------------------------------------------------------
#                                AST Node
#------------------------------------------------------------------------------        
      try:
        #If the node isn't part of a list
        if type(child_node) != list:
          #To store the values of in_assignment and right_assignment
          prev_type = in_assignment
          prev_right = self.right_assignment
          #If an attribute or function call is being made
          #As a note, both start with a Call node
          if node_type == 'Call':
            if type(child_node).__name__ == 'Name':
              in_assignment = self.context["function_call"]
            elif type(child_node).__name__ == 'Attribute':
              in_assignment = self.context["attribute"]
          #The assign node has one child that isn't a list, and it's
          #the expression being evaluated, not the variable name(s)
          if node_type == 'Assign':
            self.right_assignment = 1
          #Mostly useful later when reading file, get's aliases for
          #'import a from b' and 'import a from b as c'
          if node_type == 'ImportFrom' or node_type == 'alias':
            from_imports(node)

          args.append(self.visit(child_node,in_assignment))


          #Handles following function calls of type a.b.c()
          #If statement makes sure we're at c, and not just at a.b
          if in_assignment == self.context['attribute'] and \
          not prev_type == self.context['function_call'] and \
          not prev_type == self.context['attribute']:
            #Path of the function
            handling_attributes()
          in_assignment = prev_type
          self.right_assignment = prev_right
        
        
#------------------------------------------------------------------------------
#                                List of AST Nodes
#------------------------------------------------------------------------------        
        else:
          listArgs = []
          for elm in child_node:

            #Special case because of partial
            #node.args[0] represents the function being called by partial
            if node_type == 'Call':
#              try:
#                if elm == node.args[0] and node.func.id == 'partial':
                  if type(elm).__name__ == 'Name':
                    in_assignment = self.context["function_call"]
                  elif type(elm).__name__ == 'Attribute':
                    in_assignment = self.context["attribute"]
#              except:
#                pass          
            #The assign node only has one child that's a list, and it's
            #the names of the variables that are being assigned to
            #This is (only) important to catch assigning an attribute 
            if node_type == 'Assign':
              in_assignment = self.context["assignment"]
            

            listArgs.append(self.visit(elm,in_assignment))

            
            #Note: some subtle reason for setting to 0 instead of restoring?
            in_assignment = 0
          args += [listArgs]


#------------------------------------------------------------------------------
#                                Literal (int, string, etc.)
#------------------------------------------------------------------------------        
      except:
        if in_assignment == self.context["assignment"]:
          #Checking if the variable is global or local
          if not str(child_node) in self.var_list:
            if type(node).__name__ == 'Name':
              self.evil_function = str(child_node) + " is a global variable"
        elif in_assignment == self.context["function_call"]:
          #Means the code looked like you were defining a variable when it was
          #really a function
          try:
            curr_func = self.getFunction(child_node)
            self.curr_function[curr_func] = 1
            if type(curr_func).__name__ == 'classobj':#$ or \
#            type(curr_func).__name__ == 'builtin_function_or_method':
              if (self.right_assignment):
#                pass
                self.evil_function = 'a complicated object was created'
            else:
              self.curr_function[curr_func] = 1
          except:
            pass
        elif in_assignment == self.context['attribute']:
          self.curr_path += '.' + str(child_node)
        if self.right_assignment == 1:
          pass #Note1: see if this is a global variable and should be added
          #to a list
        
        args.append(str(child_node))
    
    return paranodes(node, args)

def readFile(file_name):
  try:
    f = open(file_name,"r")
    out_string = f.readlines()
    f.close()
  except IOError:
    print 'Error opening %s' % file_name
    return ''
  return out_string

def paranodes(node, args):
  node_type = type(node).__name__
  if (node_type == 'Name'):
    print "var(",args[0],")"
    if args[0] == 'True':
      return c_void_p(libtest.mk_bool(1))
    elif args[0] == 'False':
      return c_void_p(libtest.mk_bool(0))      
    else:
      return c_void_p(libtest.mk_var(c_char_p(args[0]),None))
  elif (node_type == 'Assign'):
    print "def(",node.targets[0].id,",",args[1],")"    
    #Note: currently doesn't allow for multiple assignment
    return c_void_p(libtest.mk_def(c_char_p(node.targets[0].id),args[1],0))
  elif (node_type == 'BinOp'):
    print "app(",type(node.op).__name__,",[",args[0],",",args[2],"])"
    bin_args = list_to_ctypes_array([args[0],args[2]],c_void_p)
    operation = builtin_primitives[type(node.op).__name__]
    return c_void_p(libtest.mk_app(operation,bin_args,2,None))
  elif (node_type == 'UnaryOp'):
    print "app(",type(node.op).__name__,",[",args[1],"])"
    unary_arg = list_to_ctypes_array(args[1],c_void_p)
    operation = builtin_primitives[type(node.op).__name__]
    return c_void_p(libtest.mk_app(operation,unary_arg,1,None))
  elif (node_type == 'Compare'):
    print "app(",type(node.ops[0]).__name__,",[",args[0],",",args[2][0],"])"
    comp_args = list_to_ctypes_array([args[0],args[2][0]],c_void_p)
    operation = builtin_primitives[type(node.ops[0]).__name__]
    return c_void_p(libtest.mk_app(operation,comp_args,2,None))    
  elif (node_type == 'Num'):
    num = eval(args[0])
    if type(num) == int:
      print "int32(",num,")"
      return c_void_p(libtest.mk_int32_paranode(num,None))
    elif type(num) == float:
      print "float32(",num,")"
      return c_void_p(libtest.mk_float_paranode(c_float(num),None))
  elif (node_type == 'Call'):
    #Note: special case for partial
    print "app(",node.func.id,",",args[1],")"
    fun_name = node.func.id
    if fun_name == 'partial':
      fun_name = node.args[0].id
      args[1] = args[1][1:]
      print fun_name,args[1]
    #Should be in a general function later
    try:
      exec "fun_ref = np.%s" % fun_name
      fun_node = safe_functions[fun_ref]
#      print "FOUND: np.%s" % fun_name
    except:
      try:
        exec "fun_ref = %s" % fun_name
        fun_node = safe_functions[fun_ref]
      except:
#      exec "fun_ref = %s" % fun_name
        fun_node = c_void_p(libtest.mk_var(c_char_p(fun_name),None))
#      print "FUNCTION CALL NAME:", fun_name
    fun_args = list_to_ctypes_array(args[1],c_void_p)
    return c_void_p(libtest.mk_app(fun_node,fun_args,len(args[1]),None))
  elif (node_type == 'Return'):
    #Note: Always just the 1st argument?
    return args[0]
  elif (node_type == 'Module'):
    print "block(",args[0],")"
    numArgs = len(args[0])
    block = list_to_ctypes_array(args[0], c_void_p)
    return c_void_p(libtest.mk_block(block,numArgs,None))
  elif (node_type == 'While'):
    print "while(",args[0],",block(",args[1],"))"
    numStmt = len(args[1])
    block_args = list_to_ctypes_array(args[1],c_void_p)
    block = c_void_p(libtest.mk_block(block_args,numStmt,None))
    return c_void_p(libtest.mk_whileloop(args[0],block,None))
  elif (node_type == 'Subscript'):
    print "app(",type(node.slice).__name__,",[",args[0],",",args[1],"])"
    operation = builtin_primitives[type(node.slice).__name__]
    array_args = list_to_ctypes_array([args[0],args[1]],c_void_p)
    return c_void_p(libtest.mk_app(operation,array_args,2,None))

  elif (node_type == 'Index'):
    return args[0]
  else:
    print "Not handled:",node_type
    return ''

def functionInfo(function_obj):
  #get the information for the sourcecode
  try:
    code_info = function_obj.__code__
  except:
    return ''
  args = code_info.co_varnames[:code_info.co_argcount]
  variable_list = list(code_info.co_varnames)
  #Get the actual text of the file that contains the function
  code_text = readFile(str(code_info.co_filename))
  #Keeps track of the current line of the code being read
  line_no = 1
  #Keeps track of the size of a tab in the code
  size_tab = 0
  #Contains only code inside the function
  out_string = ""
  #tabs can be spaces or \t characters
  tab_char = ' '
  #Keeps tracks of blank or comment lines at the beginning of the function
  #Note: switch to simple boolean?
  first_comments = 0
  for line in code_text:
    #Note: need to change now that function decorators mess up the first lineno
    #if it's the first line of meaningful code
    if line_no == code_info.co_firstlineno+2+first_comments:
      if line.isspace() or line.strip()[0] == '#':
        first_comments += 1
    if line_no > code_info.co_firstlineno +1+ first_comments:
      #Special case for the first line of code
      if line_no == code_info.co_firstlineno + 2 + first_comments:
        tab_char = line[0]
        for c in line:
          if c == tab_char:
            size_tab += 1
          else:
            break
      if line.isspace():
        pass
      elif line[:size_tab] == tab_char*size_tab:
        out_string += line[size_tab:]
      elif line[:size_tab].__contains__('#'):
        pass
      else:
        break
    line_no += 1
  return args, out_string, variable_list

def fun_visit(func,name = ''):
  fun_info = functionInfo(func)
  if fun_info:
    node = ast.parse(fun_info[1])
    AST = ASTCreator(name)
  #Note: Put in __init?
    AST.var_list = fun_info[2]
    AST.func_name = func.__module__ + "." + func.__name__
    finalTree = AST.visit(node,0)
    if AST.npArrayEvil and AST.npArrayEvil != 'Safe':
      AST.evil_function = AST.npArrayEvil
    if AST.evil_function:
      print "This function is evil because:",AST.evil_function
      return
    else:
      if printing:
        AST.prettyPrint()
      #Ignoring global variables, would use this
      ######################print "VARS",AST.var_list and fun_info[2]
      emptyList = c_char_p * 0
      varList = c_char_p * len(fun_info[0])
      vars = varList()
      for index in range(len(fun_info[0])):
        vars[index] = fun_info[0][index]
#      print "FINALTREE",finalTree
      PrettyAST.printAst(node)

#    Recursively call fun_visit on functions here


      num_func = len(AST.curr_function)
      #Note: actual global variables (non-functions) will be initialized later
      global_vars = []
      for key in AST.curr_function.keys():
        if not (function_asts.has_key(key)):
          fun_visit(str(key.__module__),key)
      globals_list = list_to_ctypes_array(global_vars,c_char_p)
      function_globals[func] = global_vars
      funID = c_int(libtest.register_untyped_function(c_char_p(func.__name__),
                                                      globals_list,
                                                      len(global_vars),
                                                      vars,
                                                      len(fun_info[0]),
                                                      finalTree))
      print "REGISTERED",func.__name__
      #Note, shouldn't return this
      #should add to a dictionary that associates function references
      #To there completely built ASTs
      function_asts[func] = funID
      #Note: will be changed
      #1 list for functions already found (including builtin)
      #1 list for functions that need to be handled
      for key in AST.curr_function.keys():
        if numpyPrimitives.get(key):
          print key.__name__,"is a numpy function"
          AST.func_handled[key] = 1
        elif functoolsPrimitives.get(key):
          print key.__name__,"is a functools function"
          AST.func_handled[key] = 1
        elif (not AST.func_handled.has_key(key)):
          print 'Now visiting:', key.__name__
          AST.func_handled[key] = 1
          fun_visit(str(key.__module__),key)
          print 'Finished visiting:', key.__name__


def runFunction(func,args):
  num_args = len(args)
  input_arr = []
  INPUTLIST = c_void_p*num_args
  EMPTYLIST = c_int * 0
  ONELIST = c_int * 1
  inputs = INPUTLIST()
#  input_data = [0] * num_args
#  input_shape = [0] * num_args
  for i in range(num_args):
    arg = args[i]
    try: #if Numpy array
      if len(arg.shape) > 1:
        arg1 = np.transpose(arg)
        arg = arg1.copy()
#        print arg,arg.base
#        return
      input_shape = arg.ctypes.shape_as(c_int)
      nbytes = arg.nbytes
      if arg.dtype == np.int32:
        input_data = arg.ctypes.data_as(POINTER(c_int))
        elmt_type = c_void_p(libtest.mk_scalar(7)) #Int32T
      elif arg.dtype == np.float32:
        input_data = arg.ctypes.data_as(POINTER(c_float))
        elmt_type = c_void_p(libtest.mk_scalar(11)) #Float32T
      elif arg.dtype == np.float64:
        input_data = arg.ctypes.data_as(POINTER(c_double))
        elmt_type = c_void_p(libtest.mk_scalar(12)) #Float64T
      else:
        print "not handled?",arg.dtype
      for z in range(len(arg.shape)):
        elmt_type = c_void_p(libtest.mk_vec(elmt_type))
        
#      print "ELEMENTS",input_data,elmt_type,input_shape,len(arg.shape),nbytes
      inputs[i] = c_void_p(libtest.mk_host_array(input_data,
                                                 elmt_type,
                                                 input_shape,
                                                 len(arg.shape),
                                                 nbytes))
    except: #Scalar
      if type(arg) == int:
        inputs[i] = c_void_p(libtest.mk_int32(arg))
      elif type(arg) == float:
        print "FLOAT INPUT",arg
        inputs[i] = c_void_p(libtest.mk_float32(c_float(arg)))

      elif type(arg) == np.float64:
        inputs[i] = c_void_p(libtest.mk_double64(c_double(arg)))
####  global_values = function_globals[func]
####  global_args = []
  ret = libtest.run_function(func, None, 0, inputs, num_args)
  if (ret.return_code == 0): #Success
    print "TYPE",libtest.get_dyn_type_element_type(c_void_p(ret.ret_types[0]))
    elmt_type = libtest.get_dyn_type_element_type(c_void_p(ret.ret_types[0]))
    #NOTE: WRONG numbers, should be (x-3)/4....returning as a wrong type of integer?
    if libtest.get_dyn_type_rank(c_void_p(ret.ret_types[0])) == 0:
      if elmt_type == 15:
        rslt = cast(ret.data.results[0],POINTER(c_int))
      elif elmt_type == 23:
        rslt = cast(ret.data.results[0],POINTER(c_float))
      elif elmt_type == 25:
        rslt = cast(ret.data.results[0],POINTER(c_double))
      else:
        rslt = cast(ret.data.results[0],POINTER(c_int))
      np_rslt = rslt[0]
      print np_rslt
    else:
      if elmt_type == 47:
        rslt = cast(ret.data.results[0],POINTER(c_float))
      elif elmt_type == 31:
        rslt = cast(ret.data.results[0],POINTER(c_int))
      elif elmt_type == 51:
        rslt = cast(ret.data.results[0],POINTER(c_double))
      else:
      #Not sure what to do with it yet
        rslt = cast(ret.data.results[0],POINTER(c_int))
      py_rslt = []
      ret_len = 1
      for index in range(libtest.get_dyn_type_rank(c_void_p(ret.ret_types[0]))):
        ret_len *= ret.shapes[0][index]
#      ret_len = ret.shapes[0][0]
      for index in range(ret_len):
        print rslt[index],
        py_rslt.append(rslt[index])
      print
      np_rslt = np.array(py_rslt)
    return np_rslt
  return 0

def GPU(fun):
#  print fun.__name__
  import inspect
#  print type(fun.__module__)
  function_file = inspect.getfile(fun)
#  print function_file
  function_name = str(function_file).split(".")[0]
#  print fun.func_globals
#  print function_name
#  return
#  print fun.__code__.co_filename
  if not (function_asts.has_key(fun)):
    fun_visit(fun,function_name)

  funID = function_asts[fun]
  
#  return
  def new_f(*args, **kwds):
    return runFunction(funID,args)
  return new_f
