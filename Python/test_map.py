#!/usr/bin/python
from numpy import *
from parakeet import PAR
import sys

@PAR 
def simple_map(x):
  return x * 3

def test_simple_map():
  print "Testing Scalar Map Multiply"
  sys.stdout.flush()
  x = array([1,2,3,4])
  y = simple_map(x)
  for i in range(len(x)):
    print "Expected %d, got %d" % (x[i]*3, y[i])

if __name__ == '__main__':
  test_simple_map()

