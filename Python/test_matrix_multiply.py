#!/usr/bin/python
import numpy as np
import parakeet
from parakeet import PAR

# TODO: Add support for transposed view Y.T
@PAR
def matmult(X,Y):
  return parakeet.allpairs(parakeet.dot, X, Y, axes=[0])

def test_matmult():
  x = np.random.randn(100,100)
  y = x.T
  z = matmult(x,y)
  z_o = matmult.call_original(x,y)
  print "Python: %s\nParakeet: %s" % (z_o, z)
  assert np.all(z == z_o)

if __name__ == '__main__':
  test_matmult()

