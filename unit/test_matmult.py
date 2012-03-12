#!/usr/bin/python
import numpy as np
import parakeet

@parakeet.PAR
def dot(x,y):
  return parakeet.reduce(parakeet.add, x*y)

# for now, assume Y is already transposed
@parakeet.PAR
def mm(X,Y):
  return parakeet.allpairs(dot, X, Y, axis=[0])

def test_mm():
  X = np.random.randn(1000, 1000)
  Y = np.random.randn(1000, 1000)
  parakeet_result = mm(X,Y)
  np_result = np.dot(X,y)
  assert np.sum( (parakeet_result - np_result) ** 2) <= 0.0001

if __name__ == '__main__':
  test_mm()


