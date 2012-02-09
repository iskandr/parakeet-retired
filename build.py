#!/usr/bin/python

import glob, optparse, os, shutil, subprocess, sys

parser = optparse.OptionParser(description='Parakeet Build System')

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

parser.add_option('-i', '--install_dir', help='Installation directory',
                  default=os.getenv("HOME")+'/.parakeet')

(opts, args) = parser.parse_args()
opts = opts.__dict__
print ""
print "==================================================="
print "|                Medium Sized Whale               |"
print "==================================================="
print ""
print opts 

if not os.path.exists(opts['install_dir']):
  os.mkdir(opts['install_dir'])

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
  os.chdir("../Runtime/LLVM")
  print
  print "Cleaning LLVM Runtime directory"
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
build_command = ["ocamlbuild",
                 "-cflags",
                 "-I,/usr/local/lib/ocaml",
                 "-lflags",
                 "-ccopt," + parakeet_path + "/cuda/parakeet_cuda.a," +\
                 "-ccopt,-L/usr/local/cuda/lib," +\
                 "-ccopt,-L/usr/local/cuda/lib64," +\
                 "-ccopt,-L/usr/lib/nvidia-current," +\
                 "-ccopt,-lcuda,-ccopt,-lcudart," +\
                 "-ccopt," + parakeet_path + "/FrontEnd/parakeet.a" +\
                 "-I,/usr/local/lib/ocaml",
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


print "BUILD COMMAND =", " ".join(build_command)

# Clean _build directory
for f in glob.glob("_build/*.o"):
  os.remove(f)
# Clean installation directory
for f in glob.glob(opts['install_dir'] + "/*.so"):
  os.remove(f)
for f in glob.glob(opts['install_dir'] + "/parakeetconf.xml"):
  os.remove(f)

# Clean Common directory
os.chdir("Common")
subprocess.call(["make", "clean"])
os.chdir("..")

# Clean FrontEnd directory
os.chdir("FrontEnd")
subprocess.call(["make", "clean"])
os.chdir("..")

# Clean install directory
os.chdir("install")
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

# Build LLVM Runtime
print "\n\n ****** Building LLVM Runtime ******"
os.chdir("Runtime/LLVM")
if subprocess.call(["make"]):
  print "LLVM Runtime build failed"
  sys.exit(1)
os.chdir("../..")

# Build installation
print "\n\n ****** Probing machine for hardware ******"
os.chdir("install")
if subprocess.call(["make"]):
  print "Installation build failed"
  sys.exit(1)
if subprocess.call(["./machine_probe"]):
  print "Machine probe failed"
  sys.exit(1)
shutil.move("parakeetconf.xml", opts['install_dir'])
os.chdir("..")

# Build Python Front End
os.chdir("Python")
print "Building Python Front End"
if subprocess.call(make_command):
  print "Python Front End build failed"
  sys.exit(1)
for f in glob.glob("*.o"):
  shutil.move(f, "../_build")
shutil.move("libparakeetpy.so", opts['install_dir'])
os.chdir("..")

if opts['tests']:
  print "Building tests"
  if subprocess.call(build_command + ['Tests/tests.native', '-no-hygiene']):
    print "Test build failed"
    sys.exit(1)
  print "Running tests"
  if subprocess.call('./tests.native'):
    print "Tests failed"
    sys.exit(1)
  
