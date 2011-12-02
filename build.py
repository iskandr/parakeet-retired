#!/usr/bin/python

import optparse, glob, os, shutil, subprocess, sys

# For now, I'm setting a -q flag to default to True, but once we move to
# Python, we can switch that off.
parser = optparse.OptionParser(description='Parakeet Build System')
parser.add_option('-q', action='store_true', help='Build Q front end')

parser.add_option('-p', '--python', action='store_true', default=True, 
                    help='Build Python front end')

parser.add_option('-o', '--opt', action='store_true',
                    help='Disable debug mode')

parser.add_option('-b', '--bytecode', action='store_true', 
  help = 'Compile OCaml code with ocamlc instead of ocamlopt')
  
parser.add_option('-r', '--prof', action='store_true',
                    help='Enable profiling')

parser.add_option('-t', '--tests', action='store_true',
                    help='Build and run unit tests')

parser.add_option('-c', '--clean', action='store_true',
                    help='Clean tree and exit')

(opts, args) = parser.parse_args()
opts = opts.__dict__
print ""
print "==================================================="
print "|                Medium Sized Whale               |"
print "==================================================="
print ""
print opts 

if opts['bytecode']:
  obj_suffix = 'cmo'
  obj_archive_suffix = 'cma'
  ocaml_compiler = 'ocamlc'
else:
  obj_suffix = 'cmx'
  obj_archive_suffix = 'cmxa'
  ocaml_compiler = 'ocamlopt'

make_command = [
  "make", 
  "OBJ_SUFFIX="+obj_suffix, 
  "OBJ_ARCHIVE_SUFFIX="+obj_archive_suffix, 
  "OCAML_COMPILER="+ocaml_compiler
] 
  
if opts['clean']:
  os.chdir("cuda")
  print
  print "Cleaning Cuda directory"
  print
  subprocess.call(make_command + ["clean"])
  os.chdir("../FrontEnd")
  print
  print "Cleaning FrontEnd directory"
  print
  subprocess.call(make_command + ["clean"])
  os.chdir("../Python")
  print
  print "Cleaning Python directory"
  print
  subprocess.call(make_command + ["clean"])
  os.chdir("../Q")
  print
  print "Cleaning Q directory"
  print
  subprocess.call(make_command + ["clean"])
  os.chdir("..")
  print
  print "Removing build directory"
  print
  shutil.rmtree("_build", ignore_errors=True)
  os.remove("preprocess.native")
  for f in glob.glob("*~"):
    os.remove(f)
  print
  print "All Clean!"
  print
  sys.exit(0)

# Get the path and set up the ocamlbuild command
parakeet_path = ".."
if 'PARAKEET_PATH' in os.environ:
  parakeet_path = os.environ['PARAKEET_PATH']
build_command = ["ocamlbuild", "-lflags",
                 "-ccopt," + parakeet_path + "/cuda/parakeet_cuda.a," +\
                 "-ccopt,-L/usr/local/cuda/lib," +\
                 "-ccopt,-L/usr/lib/nvidia-current," +\
                 "-ccopt,-lcuda,-ccopt,-lcudart," +\
                 "-ccopt," + parakeet_path + "/FrontEnd/parakeet.a",
                 "-pp", "camlp4o", "-ppflag", "pa_macro.cmo",
                 "-ocamlyacc", "menhir", 
                 ]

# Handle debugging
os.environ['dbg'] = '0'
if not opts['opt']:
  build_command.append("-ppflag")
  build_command.append("-DDEBUG")
  build_command.append("-cflag")
  build_command.append("-g")
  make_command.append("DEBUG=-g")
  os.environ['dbg'] = '1'

print build_command

print " ".join(build_command)

# Clean _build directory
for f in glob.glob("_build/*.o"):
  os.remove(f)
for f in glob.glob("_build/*.so"):
  os.remove(f)

# Clean Common directory
os.chdir("Common")
subprocess.call(["make", "clean"])
os.chdir("..")

# Clean FrontEnd directory
os.chdir("FrontEnd")
subprocess.call(["make", "clean"])
os.chdir("..")

# Clean Q directory
if opts['q']:
  os.chdir("Q")
  subprocess.call(["make", "clean"])
  os.chdir("..")

# Build Common
os.chdir("Common")
if subprocess.call(make_command):
  print "Parakeet Common C build failed"
  sys.exit(1)
os.chdir("..")

# Build CUDA stubs 
print "\n\n ******** Building Cuda Modules ********* "
os.chdir("cuda")
if subprocess.call(["make"]):
  print "Cuda build failed"
  sys.exit(1)
os.chdir("..")

# Build FrontEnd
print "\n\n ****** Building Parakeet Front End Interface ******"
if subprocess.call(build_command + ["Callbacks." + obj_suffix, "-no-hygiene"]):
  print "Parakeet Front End Callbacks build failed"
  sys.exit(1)
os.chdir("FrontEnd")
if subprocess.call(make_command):
  print "Parakeet Front End C build failed"
  sys.exit(1)
os.chdir("..")

# Build Q Front End
if opts['q']:
  print "Building Q Preprocessor and Q Callbacks"
  if subprocess.call(build_command +
                     ["QCallbacks." + obj_suffix, "preprocess.native", "-no-hygiene"]):
    print "Q Preprocessor build failed"
    exit(1)
  os.chdir("Q")
  print "Building Q Front End"
  if subprocess.call(make_command):
    print "Q Front End build failed"
    sys.exit(1)
  for f in glob.glob("*.o"):
    shutil.move(f, "../_build")
  shutil.move("libparakeetq.so", "../_build")
  os.chdir("..")

# Build Python Front End
if opts['python']:
  os.chdir("Python")
  print "Building Python Front End"
  if subprocess.call(make_command):
    print "Python Front End build failed"
    sys.exit(1)
  for f in glob.glob("*.o"):
    shutil.move(f, "../_build")
  shutil.move("libparakeetpy.so", "../_build")
  os.chdir("..")
