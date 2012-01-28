from parakeet import PAR
from math import *

@PAR
def e(x):
  return sqrt(x)

def test_int():
  x = e(3)
  y = sqrt(3)
  print "Expected %f, got %f" % (y, x)
  assert y = x

def test_float():
  x = e(3.0)
  y = sqrt(3.0)
  print "Expected %f, got %f" % (y, x)
  assert y == x

if __name__ == '__main__':
  test_float()
  test_int()

