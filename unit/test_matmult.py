#!/usr/bin/python
import numpy as np
import parakeet

parakeet.set_vectorize(False)
parakeet.set_multithreading(False)

@parakeet.PAR
def dot(x,y):
  return parakeet.reduce(parakeet.add, x*y)

# for now, assume Y is already transposed
@parakeet.PAR
def mm(X,Y):
  return parakeet.allpairs(dot, X, Y, axis=0)

def test_mm():
  X = np.random.randn(1000, 1000)
  #X = np.array([[1,2,3,4,5], [6,7,8,9,10]], dtype='int32')
  #X = np.random.randn(10, 10)
  #Y = np.random.randn(10, 10)
  parakeet_result = mm(X,X)
  print parakeet_result
  np_result = np.dot(X,X.T)
  print np_result
  assert np.sum( (parakeet_result - np_result) ** 2) <= 0.0001

if __name__ == '__main__':
  test_mm()


