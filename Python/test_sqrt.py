from parakeet import PAR
from math import *

def approx_eq(x, y):
  return abs(x-y) <= abs(x) * 0.000001

@PAR
def e(x):
  return sqrt(x)

def test_int():
  x = e(3)
  y = sqrt(3)
  print "Expected %f, got %f" % (y, x)
  assert approx_eq(x,y)

def test_float():
  x = e(3.0)
  y = sqrt(3.0)
  print "Expected %f, got %f" % (y, x)
  assert approx_eq(x,y)

if __name__ == '__main__':
  test_float()
  test_int()

