#!/usr/bin/python
import numpy as np
import parakeet as par 
from parakeet import PAR 

par.set_vectorize(False)


def mm(X,Y):
  return par.allpairs(par.dot, X, Y.T, axis=0)

def test_mm():
  X = np.random.randn(1000, 1000)
  parakeet_result = mm(X,X)
  print parakeet_result
  np_result = np.dot(X,X)
  print np_result
  assert np.sum( np.abs(parakeet_result - np_result)) <= 0.0001

if __name__ == '__main__':
  test_mm()


