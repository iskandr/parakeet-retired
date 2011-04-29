import ast
from ctypes import *
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
###########

#List of which built-in functions are allowed and which aren't
primitives = {"abs":1,"all":1,"any":1,"basestring":1,"bin":0,"bool":0,"bytearray":1,
              "callable":1,"chr":0,"classmethod":0,"cmp":1,"compile":0,"complex":0,
              "delattr":0,"dict":0,"dir":0,"divmod":1, "enumerate":0, "eval":0,
              "execfile":0,"file":0,"filter":0,"float":1,"format":0, "frozenset":0,
              "getattr":1,"globals":1,"hasattr":1,"hash":1,"help":0, "hex":0, "id":1,
              "input":0,"int":1,"isinstance":1, "issubcalss": 1, "iter":0, "len":1,
              "list":0, "locals":1, "long":1, "map": 1, "max":1, "memoryview":0,
              "min":1,"next":1,"object":0,"oct":0,"open":0,"ord":0,"pow":1,"print":0,
              "property":1,"range":0,"raw_input":0,"reduce":1,"reload":1,"repr":0,
              "reversed":0,"round":1,"set":0,"settattr":0,"slice":0,"sorted":0,
              "staticmethod":0,"str":0,"sum":1,"super":0,"tuple":0,"type":1,"unichr":0,
              "unicode":0,"vars":1,"xrange":0,"zip":0}


import numpy as np
import functools as ft
numpyPrimitives = {np.all:1,np.sum:1,np.argmin:1,np.mean:1}
functoolsPrimitives = {ft.partial:1}
newPrimitves = {"sum":"sum","argmin":"argmin","map":"map","mean":"mean","arange":"range","all":"all"}


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
  func_handled = {}
  def __init__(self,file_name):
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
  def getFunction(self,newNode):
    if str(newNode) in self.var_list:
      self.evil_function = "calling a variable as a function is not supported"
      print str(newNode)    
    func_str = self.function_path + '.' + str(newNode)
    try:
      exec "curr_func = %s" % func_str
    except:
      try:
        exec "curr_func = %s.__globals__['%s']" % (self.func_name , str(newNode))
        func_str = str(newNode)
      except:
        str_new_node = str(newNode)
        try:
          exec "import %s" % self.from_import[str_new_node]
          str_new_node = self.from_import[str_new_node] + "." + str_new_node
        except:
          pass
        exec "curr_func = %s" % str_new_node
        try:
          if not primitives[str(newNode)]:
            self.evil_function = "%s is an unsupported primitive" % str(newNode)
        except:
          pass #Note: Not a primitive, use this somehow?
    return curr_func
           
  def visit(self,node,in_assignment):
    #Some tests for evilness
    if type(node).__name__ == "Print":
      self.evil_function = "printing is not allowed"
    if in_assignment == self.context["assignment"]:
      if type(node).__name__ == "Attribute":
        self.evil_function = "changing a field is not allowed"
    if self.right_assignment:
      if type(node).__name__ == "Str":
        self.evil_function = "strings are not supported"
      if type(node).__name__ == "List" or type(node).__name__ == "Tuple":
        if self.npArrayEvil != "Safe":
          self.npArrayEvil = "lists and tuples are not supported outside of numpy arrays"
        #Note: do something here to check for global variables?
      if self.evil_function:
        return
    newNode = node._fields
    self.newRow()
    args = []
    #Visits all of the child nodes
    for child in node._fields:
      exec "newNode = node.%s" % child
      #Sees if the child being visited is a node or a normal dataType i.e. ints
      try:
        #If the node isn't part of a list
        if type(newNode) != type([]):
          prev_type = in_assignment
          prev_right = self.right_assignment
          #If an attribute or function call is being made
          #As a note, both start with a Call node
          if type(node).__name__ == 'Call':
            if type(newNode).__name__ == 'Name':
              in_assignment = self.context["function_call"]
            elif type(newNode).__name__ == 'Attribute':
              in_assignment = self.context["attribute"]
          #The assign node has one child that isn't a list, and it's
          #the expression being evaluated, not the variable name(s)
          if type(node).__name__ == 'Assign':
            self.right_assignment = 1
          if type(node).__name__ == 'ImportFrom':
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
          args.append(self.visit(newNode,in_assignment))
          print "Nonlist:",args
#          print "1",args
          #Handles following function calls of type a.b.c()
          #If statement makes sure we're at c, and not just at a.b
          if in_assignment == self.context['attribute'] and \
          not prev_type == self.context['function_call'] and \
          not prev_type == self.context['attribute']:
            #Path of the function
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
                if (func_str.split('.').pop() == 'array'):
                  self.npArrayEvil = "Safe"
                #If it's not a numpy array, then it's not handled by parakeet
                else:
                  self.evil_function = "a complicated object was created"
            else:
              #Otherwise, we're gonna say it's a function
              self.curr_function[curr_func] = 1
            self.curr_path = ''
          in_assignment = prev_type
          self.right_assignment = prev_right
        #If it's a list, unpack the list and visit EACH child node
        else:
          listArgs = []
          for elm in newNode:
            #Special case because of partial :S
            if type(node).__name__ == 'Call':
              try:
                if elm == node.args[0] and node.func.id == 'partial':
                  if type(elm).__name__ == 'Name':
                    in_assignment = self.context["function_call"]
                  elif type(elm).__name__ == 'Attribute':
                    in_assignment = self.context["attribute"]
              except:
                pass          
            #The assign node only has one child that's a list, and it's
            #the names of the variables that are being assigned to 
            if type(node).__name__ == 'Assign':
              in_assignment = self.context["assignment"]
            listArgs.append(self.visit(elm,in_assignment))
            print "Nonlist in list:",listArgs
 #           print "2",listArgs
            in_assignment = 0
          args += listArgs
          print "list:",args
 #         print "3",args
        #Otherwise, we still want the info for the child, it's just not a node
      except:
        #Means it's a variable name
        if in_assignment == self.context["assignment"]:
          if not str(newNode) in self.var_list:
            if type(node).__name__ == 'Name':
              self.evil_function = str(newNode) + " is a global variable"
        elif in_assignment == self.context["function_call"]:
          #Means the code looked like you were defining a variable when it was
          #really a function
          curr_func = self.getFunction(newNode)
          if type(curr_func).__name__ == 'classobj':#$ or \
#          type(curr_func).__name__ == 'builtin_function_or_method':
            if (self.right_assignment):
#              pass
              self.evil_function = 'a complicated object was created'
          else:
            self.curr_function[curr_func] = 1

        elif in_assignment == self.context['attribute']:
          self.curr_path += '.' + str(newNode)
        if self.right_assignment == 1:
          pass #Note: see if this is a global variable
        #Note: printing stuff?
        self.printLiteral(newNode)
        args.append(str(newNode))
    
    #Now that the child information is known, we can do pretty printing
    self.printNode(type(node).__name__)
    #return ''
#    print 'hi'
    x = paranodes(node, args)
    if testing == 1 and debug == 1:
      print x
#    print 'bye'
    return x

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
  print "TYPE:",type(node).__name__
  if testing == 1:
#      print "ASSIGN?",type(args),type([])
    argStr = ''
#TEMP:    print args
    for elm in args:
      if type(elm) == type([]):
        argStr += "{"
        for elm2 in elm:
          argStr += elm2 + ','
        argStr += "}"
      else:
        argStr = argStr + elm + ','
    return type(node).__name__ + "[" + argStr + "]"
  list_args = c_void_p * 2 #Note: make this more generic?
  node_type = type(node).__name__
  #print node_type
  if (node_type == 'Name'):
    return c_void_p(libtest.mk_var(c_char_p(args[0]),None))
  elif (node_type == 'Assign'):
    #NOTE: args[0] is a list, not an element, so be careful
    #print args[0], args[1], type(args[0]), type(args[1])
    #x = libtest.mk_def(args[0],args[1],0)
    return libtest.mk_def(c_char_p('x'),c_void_p(args[1]),0)
    #print 'assign created'
    #return x
  elif (node_type == 'BinOp'):
    bin_args = list_args(args[0],args[2])
    return c_void_p(libtest.mk_app(args[1],bin_args,2,None))
  elif (node_type == 'Num'):
#    x = c_void_p(libtest.mk_int32_paranode(args[0],None))
#    print type(x),x
    return c_void_p(libtest.mk_int32_paranode(int(args[0]),None))
  elif (node_type == 'Add'):
    return c_void_p(libtest.mk_scalar_op(0,None))
  elif (node_type == 'Sub'):
    return c_void_p(libtest.mk_scalar_op(1,None))
  elif (node_type == 'Mult'):
    return c_void_p(libtest.mk_scalar_op(2,None))
  #Note: Doesn't really give useful info, will be grouped together in an else
  #statement later
  elif (node_type == 'Call'):
    fun_name = call.func.id
  elif (node_type == 'Store'):
    return ''
  elif (node_type == 'Return'):
    #Note: Always just the 1st argument?
    return args[0]
  elif (node_type == 'Module'):
    BLOCKLIST = c_void_p * 1
#    print args
    return c_void_p(libtest.mk_block(BLOCKLIST(args[0]),1,None))
    #return args

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

def fun_visit(name,func):
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
      funID = c_int(libtest.register_untyped_function(c_char_p(func.__name__),emptyList(),0,vars,len(fun_info[0]),finalTree))
      return funID
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

def returnTypeInit():
  #Note: Do more automatically?
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
  libtest.get_prim.restype = c_void_p
  
def runFunction(func,args):
  num_args = len(args)
  input_arr = []
  INPUTLIST = c_void_p*num_args
  inputs = INPUTLIST()
#  input_data = [0] * num_args
#  input_shape = [0] * num_args
  for i in range(num_args):
    arg = args[i]
    arg_length = len(arg)
    input_data = arg.ctypes.data_as(POINTER(c_int))
#    SHAPELIST = c_int * 1
#    input_shape[i] = SHAPELIST(arg_length)
    input_shape = arg.ctypes.shape_as(c_int)
    scalar_int = c_void_p(libtest.mk_scalar(7)) #Int32T
    vec_int = c_void_p(libtest.mk_vec(scalar_int))
    inputs[i] = c_void_p(libtest.mk_host_array(input_data,vec_int,input_shape,1,arg_length*sizeof(c_int)))

  ret = libtest.run_function(func, None, 0, inputs, num_args)
  if (ret.return_code == 0): #Success
    rslt = cast(ret.data.results[0],POINTER(c_int))
    py_rslt = []
    ret_len = ret.shapes[0][0]
    for index in range(ret_len):
      print rslt[index],
      py_rslt.append(rslt[index])
    print
    np_rslt = np.array(py_rslt)
    return np_rslt
  return 0

def RunFunction(func,args):
  num_args = 2
  INPUTLISTT = c_void_p*2
  inputs = INPUTLISTT()
  arg = args[0]
  arg_length = len(arg)
  INPUTLIST = c_int * arg_length
  input_data = INPUTLIST()
  for index in range(arg_length):
    input_data[index] = arg[index]
#  input_data = INPUTLIST(0,1,2,3,4,5,6,7,8,9)
  SHAPELIST = c_int * 1
  input_shape = SHAPELIST(arg_length)
  scalar_int = c_void_p(libtest.mk_scalar(7)) #Int32T
  vec_int = c_void_p(libtest.mk_vec(scalar_int))
  input1 = c_void_p(libtest.mk_host_array(input_data,vec_int,input_shape,1,arg_length*sizeof(c_int)))
  arg1 = args[1]
  arg_length1 = len(arg1)
  INPUTLIST1 = c_int * arg_length1
  input_data1 = INPUTLIST1()
  for index in range(arg_length1):
    input_data1[index] = arg1[index]
  SHAPELIST1 = c_int * 1
  input_shape1 = SHAPELIST(arg_length1)
  scalar_int1 = c_void_p(libtest.mk_scalar(7))
  vec_int1 = c_void_p(libtest.mk_vec(scalar_int1))
  input2 = c_void_p(libtest.mk_host_array(input_data1,vec_int1,input_shape1,1,arg_length1*sizeof(c_int)))
  inputs[0] = input1
  inputs[1] = input2
#    input_arr.append(input)
  
#  for index in range(num_args):
#    inputs[index] = input_arr[1]
  ret = libtest.run_function(func, None, 0, inputs, num_args)
  if (ret.return_code == 0): #Success
    rslt = cast(ret.data.results[0],POINTER(c_int))
    py_rslt = []
    ret_len = ret.shapes[0][0]
    for index in range(ret_len):
      print rslt[index],
      py_rslt.append(rslt[index])
    print
    np_rslt = np.array(py_rslt)
    return np_rslt
  return 0

#if 0:
#  import KMeans
#  fun_visit('KMeans',KMeans.sqr_dist)
#Test1:
###node = ast.parse("x = 2")
#Test2:
if simpleTest:
#  import KMMeans
#  print functionInfo(KMMeans.minidx)[1]
  node = ast.parse("return x + 20")
#End Test2
  import os
  if testing == 0:
    libcude = cdll.LoadLibrary('/usr/local/cuda/lib/libcudart.so.3')
    libtest = cdll.LoadLibrary(os.getcwd() + '/../_build/libparakeetpy.so')
    libtest.parakeet_init()
    returnTypeInit()

  AST = ASTCreator('KMMeans')
###  AST.var_list = ['x']
  if testing == 1:
    AST.visit(node,0)
  if testing == 0:
    finalTree = AST.visit(node,0)
    AST.prettyPrint()
    print type(finalTree), finalTree
    emptyList = c_char_p * 0
    varList = c_char_p * 1
    Vars = varList(c_char_p("x"))
    funID = c_int(libtest.register_untyped_function(c_char_p('add2'),emptyList(),0,Vars,1,finalTree))

    runFunction(funID)
#print type(finalTree), finalTree
#funID = libtest.register_untyped_function('hi',0,0,0,0,c_void_p(finalTree))

  if AST.evil_function:
    print "This function is evil because:",AST.evil_function
  else:
    pass
    #AST.prettyPrint()
#import KMMeans
#fun_visit('KMMeans',KMMeans.kmeans)

###Test if built-in needs to be part of if statement
###Make sure x = abs(y) works if built-in DOES have to be part of if statement

def GPU(fun):
#  print fun.__name__
#  print fun.__code__.co_filename
  global libtest
  libcude = cdll.LoadLibrary('/usr/local/cuda/lib/libcudart.so.3')
  libtest = cdll.LoadLibrary(os.getcwd() + '/../_build/libparakeetpy.so')
  libtest.parakeet_init()
  returnTypeInit()
  funID = fun_visit('add.py',fun)
  def new_f(*args, **kwds):
    return runFunction(funID,args)
  return new_f
