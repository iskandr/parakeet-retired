#!/usr/bin/python
import numpy as np
from parakeet import PAR

@PAR
def idx(X, i):
  return X[i]

def test_idx():
  X = np.array([1,2,3])
  for i in xrange(len(X)):
    output = idx(X, i)
    print "Expected X[%d] == %d, got %d" % (i, i+1,output)
    assert output == X[i]

@PAR
def array_literal():
  return np.array([53,2,-302, 1])

def test_array_literal():
  a = array_literal()
  print a, a.shape, a.strides
  expected = np.array([53,2,-302,1])
  print "Expected ", expected, " got: ", a
  assert np.all(a == expected)

if __name__ == '__main__':
  test_idx()
  test_array_literal()

