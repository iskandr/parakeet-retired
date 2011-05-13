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
function_names = {}
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
def get_parakeet_lib():
  parlib = cdll.LoadLibrary(os.getcwd() + '/../_build/libparakeetpy.so')
  parlib.parakeet_init()

  #NOTE: can set default to c_void_p, much less initialization?
  parlib.mk_def.restype = c_void_p
  parlib.mk_int32_paranode.restype = c_void_p
  parlib.mk_int64_paranode.restype = c_void_p
  parlib.mk_var.restype = c_void_p
  parlib.mk_scalar_op.restype = c_void_p
  parlib.mk_app.restype = c_void_p
  parlib.mk_lam.restype = c_void_p
  parlib.mk_block.restype = c_void_p
#  parlib.mk_scalar.restype = c_void_p
  parlib.mk_vec.restype = c_void_p
  parlib.mk_host_array.restype = c_void_p
  parlib.register_untyped_function.restype = c_int
  parlib.run_function.restype = return_val_t
  parlib.mk_int32.restype = c_void_p
  parlib.mk_float32.restype = c_void_p
  parlib.mk_float64.restype = c_void_p
  parlib.mk_whileloop.restype = c_void_p
  parlib.mk_bool.restype = c_void_p
  parlib.mk_bool_paranode.restype = c_void_p
  parlib.get_prim.restype = c_void_p
  parlib.mk_float_paranode.restype = c_void_p
  parlib.mk_double_paranode.restype = c_void_p
  parlib.mk_bool.restype = c_void_p
  parlib.get_dyn_type_element_type.restype = c_int

  # get global values for parakeet types
  parlib.bool_t = c_int.in_dll(parlib, "parakeet_bool_t").value
  parlib.char_t = c_int.in_dll(parlib, "parakeet_char_t").value
  parlib.int32_t = c_int.in_dll(parlib, "parakeet_int32_t").value
  parlib.int64_t = c_int.in_dll(parlib, "parakeet_int64_t").value
  parlib.float32_t = c_int.in_dll(parlib, "parakeet_float32_t").value  
  parlib.float64_t = c_int.in_dll(parlib, "parakeet_float64_t").value    
  return parlib 

def ast_prim(sym):
  return c_void_p(parlib.get_prim(sym))

#libcude = cdll.LoadLibrary('/usr/local/cuda/lib/libcudart.so.3')
parlib = get_parakeet_lib()

###########

safe_functions = {np.all:ast_prim('all'),
                  np.arange:ast_prim('range'),
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
  def __init__(self,file_name,function_globalss):
    self.function_globals_variables = function_globalss
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
    func_str = self.function_path + '.' + str(node)
    try:
      exec "curr_func = %s" % func_str
      print "Not in except block"
    except:
      try:
        exec "curr_func = np.%s" %str(node)
      except:
        try:
          exec "curr_func = %s.%s"%(self.file_name,str(node))
        except:
          print "Failed trying to find the function",str(node)

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
#        self.curr_function[curr_func] = 1
        if (func_str.split('.').pop() == 'array'):
          self.npArrayEvil = "Safe"
        #If it's not a numpy array, then it's not handled by parakeet
        else:
          self.evil_function = "a complicated object was created"
#    else:
#      #Otherwise, we're gonna say it's a function
#      self.curr_function[curr_func] = 1
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
#            self.curr_function[curr_func] = 1
            if type(curr_func).__name__ == 'classobj':#$ or \
#            type(curr_func).__name__ == 'builtin_function_or_method':
              if (self.right_assignment):
#                pass
                self.evil_function = 'a complicated object was created'
#            else:
#              self.curr_function[curr_func] = 1
          except:
            pass
        elif in_assignment == self.context['attribute']:
          self.curr_path += '.' + str(child_node)
        if self.right_assignment == 1:
          pass #Note1: see if this is a global variable and should be added
          #to a list
        
        args.append(str(child_node))
    
    return paranodes(node, args,self.function_globals_variables,self)

def readFile(file_name):
  try:
    f = open(file_name,"r")
    out_string = f.readlines()
    f.close()
  except IOError:
    print 'Error opening %s' % file_name
    return ''
  return out_string

def paranodes(node, args,function_globals_variables,ASTInst):
  node_type = type(node).__name__
  if (node_type == 'Name'):
    if args[0] == 'True':
      print "bool(False)"
      return c_void_p(parlib.mk_bool_paranode(1,None))
    elif args[0] == 'False':
      print "bool(False)" 
      return c_void_p(parlib.mk_bool_paranode(0,None))      
    else:
      print "var(",args[0],")"
      return c_void_p(parlib.mk_var(c_char_p(args[0]),None))
  elif (node_type == 'Assign'):
    print "def(",node.targets[0].id,",",args[1],")"    
    #Note: currently doesn't allow for multiple assignment
    return c_void_p(parlib.mk_def(c_char_p(node.targets[0].id),args[1],0))
  elif (node_type == 'BinOp'):
    print "app(",type(node.op).__name__,",[",args[0],",",args[2],"])"
    bin_args = list_to_ctypes_array([args[0],args[2]],c_void_p)
    operation = builtin_primitives[type(node.op).__name__]
    return c_void_p(parlib.mk_app(operation,bin_args,2,None))
  elif (node_type == 'UnaryOp'):
    print "app(",type(node.op).__name__,",[",args[1],"])"
    unary_arg = list_to_ctypes_array([args[1]],c_void_p)
    operation = builtin_primitives[type(node.op).__name__]
    return c_void_p(parlib.mk_app(operation,unary_arg,1,None))
  elif (node_type == 'Compare'):
    print "app(",type(node.ops[0]).__name__,",[",args[0],",",args[2][0],"])"
    comp_args = list_to_ctypes_array([args[0],args[2][0]],c_void_p)
    operation = builtin_primitives[type(node.ops[0]).__name__]
    return c_void_p(parlib.mk_app(operation,comp_args,2,None))    
  elif (node_type == 'Num'):
    num = eval(args[0])
    if type(num) == int:
      print "int32(",num,")"
      return c_void_p(parlib.mk_int32_paranode(num,None))
    elif type(num) == float:
      print "float32(",num,")"
      return c_void_p(parlib.mk_float_paranode(c_float(num),None))
  elif (node_type == 'Call'):
    #Note: special case for partial

    if type(node.func).__name__ == 'Name':
      print "app(",node.func.id,",",args[1],")"
      fun_name = node.func.id
      if fun_name == 'partial':
        fun_name = node.args[0].id
        args[1] = args[1][1:]
        print "PARTIAL:",fun_name,args[1]
      try:
        fun_ref = function_globals_variables[fun_name]
      except:
        #Either couldn't find it, or it's a built-in
        try:
          #Note: on different versions, might be __builtins__.__dict__[]
          fun_ref = __builtins__[fun_name]
          print "found python built-in",fun_name
        except:
          print "ERROR: Couldn't find:",fun_name
      ASTInst.curr_function[fun_ref] = 1
      try:
#        print "TEST1",fun_ref, np.mean
#        print "TEST2",safe_functions
        fun_node = safe_functions[fun_ref]
        print "found built-in",fun_node
      except:
  #        print "NAME", function_names, fun_ref,np.mean
        print "MODULE NAME:",fun_ref.__module__
        try:
          fun_node = c_void_p(parlib.mk_var(c_char_p(fun_ref.__module__ + 
                                                      "." + 
                                                      function_names[fun_ref]),
                                                      None))
          print "THE NAME IS:",function_names[fun_ref]
        except:
          print "THE NAME IS:",fun_name
          fun_node = c_void_p(parlib.mk_var(c_char_p(fun_ref.__module__ + 
                                                      "." + fun_name),None))          
#      print "creating ARGS",args,"for",fun_name
      fun_args = list_to_ctypes_array(args[1],c_void_p)
#      print "created args"
      num_args = len(args[1])
    elif type(node.func).__name__ == 'Attribute':
      module_list = []
      next_node = node.func
      fun_name = node.func.attr
      while type(next_node.value).__name__ == 'Attribute':
        next_node = next_node.value
#        print type(next_node.value).__name__, next_node.attr
        module_list = [next_node.attr] + module_list
#      print "MODULE_LIST",module_list,next_node.value.id
      next_modu = function_globals_variables[next_node.value.id]
      for modu in module_list:
        next_modu = next_modu.__dict__[modu]
#      print "Final module",next_modu,fun_name
      fun_ref = next_modu.__dict__[fun_name]
      ASTInst.curr_function[fun_ref] = 1
      try:
        fun_node = safe_functions[fun_ref]
#        print "fun_node was safe"
      except:
        try:
          fun_node = c_void_p(parlib.mk_var(c_char_p(fun_ref.__module__ + "." + function_names[fun_ref]),None))
        except:
          fun_node = c_void_p(parlib.mk_var(c_char_p(fun_ref.__module__ + "." + fun_name),None))
#        print "fun_node made of",fun_name
      fun_args = list_to_ctypes_array(args[2],c_void_p)
      num_args = len(args[2])

    else:
      print "Invalid node?",type(node.func).__name__
    return c_void_p(parlib.mk_app(fun_node,fun_args,num_args,None))
  elif (node_type == 'Return'):
    #Note: Always just the 1st argument?
    return args[0]
  elif (node_type == 'Module'):
    print "block(",args[0],")"
    numArgs = len(args[0])
    block = list_to_ctypes_array(args[0], c_void_p)
    return c_void_p(parlib.mk_block(block,numArgs,None))
  elif (node_type == 'While'):
    print "while(",args[0],",block(",args[1],"))"
    numStmt = len(args[1])
    block_args = list_to_ctypes_array(args[1],c_void_p)
    block = c_void_p(parlib.mk_block(block_args,numStmt,None))
    return c_void_p(parlib.mk_whileloop(args[0],block,None))
  elif (node_type == 'Subscript'):
    print "app(",type(node.slice).__name__,",[",args[0],",",args[1],"])"
    operation = builtin_primitives[type(node.slice).__name__]
    array_args = list_to_ctypes_array([args[0],args[1]],c_void_p)
    return c_void_p(parlib.mk_app(operation,array_args,2,None))

  elif (node_type == 'Index'):
    return args[0]
  elif (node_type == 'Attribute'):
    return args[1]
  else:
    print "Not handled:",node_type
    return node_type

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
  extra_lines = 0
  for line in code_text:
    #Note: need to change now that function decorators mess up the first lineno
    #if it's the first line of meaningful code
    if line_no == code_info.co_firstlineno + extra_lines:
      try:
        if line.split()[0] == 'def':
          pass
        else:
          extra_lines += 1
      except:
        extra_lines+= 1
    if line_no == code_info.co_firstlineno+1+first_comments+extra_lines:
      if line.isspace() or line.strip()[0] == '#':
        first_comments += 1
    if line_no > code_info.co_firstlineno + first_comments+extra_lines:
      #Special case for the first line of code
      if line_no == code_info.co_firstlineno+  1 + first_comments+extra_lines:
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
    AST = ASTCreator(name,func.func_globals)
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


      #Note: actual global variables (non-functions) will be initialized later
      global_vars = []
      print "FUNCTIONS:",AST.curr_function.keys()
      for key in AST.curr_function.keys():
        if not (function_asts.has_key(key)) and not (function_names.has_key(key)):
          print "Visiting",key.__name__
          fun_visit(key,str(key.__module__))
          print "Visited",key.__name__

      globals_list = list_to_ctypes_array(global_vars,c_char_p)
      function_globals[func] = global_vars
      funID = c_int(parlib.register_untyped_function(c_char_p(
                                                       func.__module__ + "." +
                                                       func.__name__),
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
#      for key in AST.curr_function.keys():
#        if numpyPrimitives.get(key):
#          print key.__name__,"is a numpy function"
#          AST.func_handled[key] = 1
#        elif functoolsPrimitives.get(key):
#          print key.__name__,"is a functools function"
#          AST.func_handled[key] = 1
#        elif (not AST.func_handled.has_key(key)):
#          print 'Now visiting:', key.__name__
#          AST.func_handled[key] = 1
#          fun_visit(str(key.__module__),key)
#          print 'Finished visiting:', key.__name__



numpy_type_to_ctype = { 
    np.int32: c_int32, 
    np.int64: c_int64, 
    np.float32: c_float,
    np.float64: c_double, 
    np.bool: c_bool
  }

numpy_type_to_parakeet_type = {
    np.int32: parlib.int32_t, 
    np.int64: parlib.int64_t, 
    np.float32: parlib.float32_t,
    np.float64: parlib.float64_t, 
    np.bool: parlib.bool_t
  }

parakeet_type_to_ctype = {
    parlib.int32_t: c_int32,
    parlib.int64_t: c_int64,  
    parlib.float32_t: c_float, 
    parlib.float64_t: c_double,
    parlib.bool_t: c_int,
    parlib.char_t: c_char
  }
  
# given a numpy array or a scalar, construct 
# the equivalent parakeet value 
def python_value_to_parakeet(arg):
  if isinstance(arg, np.ndarray):
    print arg.flags
    rank = len(arg.shape)
    input_shape = arg.ctypes.shape_as(c_int32)
    if rank > 1 and not arg.flags['C_CONTIGUOUS']:
      # until we have a proper interface for telling parakeet this data is 
      # column-major, we have to manually transpose it  
      arg = np.transpose(arg).copy()
    np_type = arg.dtype.type
    if (np_type not in numpy_type_to_ctype) or (np_type not in numpy_type_to_parakeet_type):
      raise Exception("Numpy element type unsupported: " + str(np_type))
    
    ctype = numpy_type_to_ctype[np_type]
    parakeet_type = numpy_type_to_parakeet_type[np_type]
    data_ptr = arg.ctypes.data_as(POINTER(ctype))
    #print arg, "\n", ctype, "\n"
    for i in range(np.size(arg)):
      print data_ptr[i]
    # recursively construct Vec (Vec (... elementType ...)) 
    for z in range(len(arg.shape)):
      parakeet_type = c_void_p(parlib.mk_vec(parakeet_type))
    parakeetVal = parlib.mk_host_array(data_ptr, parakeet_type, input_shape, rank, arg.nbytes)
    return c_void_p(parakeetVal)
  elif np.isscalar(arg):
    if type(arg) == int:
      return c_void_p(parlib.mk_int32(arg))
    elif type(arg) == float:
      return c_void_p(parlib.mk_float32(c_float(arg)))
    elif type(arg) == np.float64:
      return c_void_p(parlib.mk_double64(c_double(arg)))
  else:
    raise Exception("Input not supported by Parakeet: " + str(arg)) 


def array_from_memory(pointer,shape,dtype):
  from_memory = ctypes.pythonapi.PyBuffer_FromReadWriteMemory
  from_memory.restype = ctypes.py_object
  arr = np.empty(shape=shape,dtype=dtype)
  arr.data = from_memory(pointer,arr.nbytes)
  return arr
 
def parakeet_value_to_python(data, shape_ptr, ty): 
  parakeet_elt_type = parlib.get_dyn_type_element_type(ty)
  print "TYPE", parakeet_elt_type
  print parakeet_elt_type, parlib.float32_t
  c_elt_type = parakeet_type_to_ctype[parakeet_elt_type]
  rank = parlib.get_dyn_type_rank(ty)  
  if rank == 0:
    result_ptr = cast(data,POINTER(c_elt_type))
    return result_ptr[0]
  else:
    shape = []
    nelts = 1
    for index in range(rank): 
      dim = shape_ptr[index]
      nelts *= dim
      shape.append(dim)
    print "SHAPE: ", shape
    array_type = c_elt_type * nelts
    result_array = array_type.from_address(data)
    for i in range(nelts):
      print result_array[i]
    np_result = np.ctypeslib.as_array(result_array)
    np_result.shape = shape 
    # hack around Parakeet's support for only row-major data 
    #if rank == 2:
    #  np_result = np.reshape(np_result.transpose(), shape)
    #  print np_result
    #  print np_result.shape
    #  print np_result.flags
      #np_result.shape = shape
    return np_result

def runFunction(func,args):
  num_args = len(args)
  input_arr = []
  INPUTLIST = c_void_p*num_args
  EMPTYLIST = c_int * 0
  ONELIST = c_int * 1
  inputs = INPUTLIST()
  for i in range(num_args):
    inputs[i] = python_value_to_parakeet(args[i])
  ret = parlib.run_function(func, None, 0, inputs, num_args)
  if (ret.return_code == 0): #Success
    # for now limited to 1 result values 
    data = ret.data.results[0]
    ty = c_void_p(ret.ret_types[0])
    shape = ret.shapes[0]
    return parakeet_value_to_python(data, shape, ty)
  else :
    raise Exception("run_function failed")

def GPU(fun):
#  print fun.__name__
  import inspect
#  print type(fun.__module__)
  function_file = inspect.getfile(fun)
#  print function_file
  function_name = str(function_file).split(".")[0]
  if not (function_asts.has_key(fun)):
    fun_visit(fun,function_name)

  funID = function_asts[fun]
  
#  return
  def new_f(*args, **kwds):
    return runFunction(funID,args)
  function_names[new_f] = fun.__name__
  new_f.__module__ = fun.__module__
  return new_f
