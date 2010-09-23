#!/usr/bin/python

import glob, os, subprocess

# Clean the results directory
tmp_files = glob.glob("tmp/*")
for f in tmp_files:
  os.remove(f)

# Run the tests
num_passed = 0
num_failed = 0
tests = glob.glob("test_*.q")
for test in tests:
  subprocess.check_call(["q", test])
  name = test[5:-2]
  f = open("tmp/" + name + "_rslt")
  rslt = f.read(1)
  f.close()
  os.remove("tmp/" + name + "_rslt")
  if (rslt == "1"):
    num_passed += 1
    print "Test " + name + " PASSED!"
  else:
    num_failed += 1
    print "Test " + name + " FAILED!"

print "\n\nTests Summary:"
print "________________"
print num_passed, " out of ", num_passed+num_failed, " tests passed."

