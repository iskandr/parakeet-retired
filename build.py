#!/usr/bin/python2.7

import argparse, glob, os, shutil, subprocess, sys

# For now, I'm setting a -q flag to default to True, but once we move to
# Python, we can switch that off.
parser = argparse.ArgumentParser(description='Parakeet Build System')
parser.add_argument('-q', action='store_true', help='Build Q front end')
parser.add_argument('-p', '--python', action='store_true',
                    help='Build Python front end')
parser.add_argument('-d', '--debug', action='store_true',
                    help='Enable debug mode')
parser.add_argument('-r', '--prof', action='store_true',
                    help='Enable profiling')
parser.add_argument('-t', '--tests', action='store_true',
                    help='Build and run unit tests')

args = parser.parse_args().__dict__
print args

print ""
print "==================================================="
print "|                Medium Sized Whale               |"
print "==================================================="
print ""

# Get the path and set up the ocamlbuild command
pq_path = "../libpq"
if 'PQ_PATH' in os.environ:
  pq_path = os.environ['PQ_PATH']
build_command = ["ocamlbuild", "-lflags",
                 "-ccopt," + pq_path + "/libpq_stubs.o," +\
                 "-ccopt," + pq_path + "/cuda_stubs.cu_o," +\
                 "-ccopt," + pq_path + "/base.o," +\
                 "-ccopt,-L/usr/local/cuda/lib64/," +\
                 "-ccopt,-L/usr/lib/nvidia-current," +\
                 "-ccopt,-lcuda,-ccopt,-lcudart," +\
                 "-ccopt," + pq_path + "/../FrontEnd/parakeet.a",
                 "-pp", "camlp4o", "-ppflag", "pa_macro.cmo",
                 "-ocamlyacc", "menhir"]

# Handle debugging
make_command = ["make"]
os.environ['dbg'] = '0'
if args['debug']:
  print "Debug mode"
  build_command.append("-ppflag")
  build_command.append("-DDEBUG")
  make_command.append("DEBUG=-g")
  os.environ['dbg'] = '1'

print " ".join(build_command)

# Clean _build directory
for f in glob.glob("_build/*.o"):
  os.remove(f)
for f in glob.glob("_build/*.so"):
  os.remove(f)

# Clean FrontEnd directory
os.chdir("FrontEnd")
subprocess.call(["make", "clean"])
os.chdir("..")

# Clean Q directory
if args['q']:
  os.chdir("Q")
  subprocess.call(["make", "clean"])
  os.chdir("..")

# Build libpq (TODO: Rename libpq)
print "\n\n ******** Building LibPQ ********* "
os.chdir("libpq")
if subprocess.call(["make"]):
  print "LibPQ build failed"
  sys.exit(1)
os.chdir("..")

# Build FrontEnd
print "\n\n ****** Building Parakeet Front End Interface ******"
if subprocess.call(build_command + ["Callbacks.cmx", "-no-hygiene"]):
  print "Parakeet Front End Callbacks build failed"
  sys.exit(1)
os.chdir("FrontEnd")
if subprocess.call(make_command):
  print "Parakeet Front End C build failed"
  sys.exit(1)
os.chdir("..")

# Build Q Front End
if args['q']:
  print "Building Q Preprocessor and Q Callbacks"
  if subprocess.call(build_command +
                     ["QCallbacks.cmx", "preprocess.native", "-no-hygiene"]):
    print "Q Preprocessor build failed"
    exit(1)
  os.chdir("Q")
  print "Building Q Front End"
  if subprocess.call(make_command):
    print "Q Front End build failed"
    sys.exit(1)
  for f in glob.glob("*.o"):
    shutil.move(f, "../_build")
  shutil.move("parakeetq.so", "../_build")
  os.chdir("..")

# Build Python Front End
if args['python']:
  #if subprocess.call(build_command +
  #                   ["QCallbacks.cmx", "preprocess.native", "-no-hygiene"]):
  #  print "Q Preprocessor build failed"
  #  exit(1)
  os.chdir("Python")
  print "Building Python Front End"
  if subprocess.call(make_command):
    print "Python Front End build failed"
    sys.exit(1)
  for f in glob.glob("*.o"):
    shutil.move(f, "../_build")
  shutil.move("parakeetpy.so", "../_build")
  os.chdir("..")
