#!/usr/bin/python
from parakeet import PAR
from math import *

@PAR
def e(x):
  return sqrt(log(x))

def test_math():
  x = e(3)
  y = sqrt(log(3))
  print "Expected %f, got %f" % (y, x)
  assert abs(y - x) < 0.00001

if __name__ == '__main__':
  test_math()

