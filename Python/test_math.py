from parakeet import PAR
from math import *

@PAR
def e(x):
  return sqrt(log(x))

def test_math():
  x = e(3)
  y = sqrt(log(x))
  print "Expected %f, got %f" % (y, x)
  assert y == x

if __name__ == '__main__':
  test_math()

