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
    print "Expected X[%d] == 1, got %d" % (i, output)
    assert output == X[i]


@PAR
def array_literal():
  return np.array([33, 27])

def test_array_literal():
  a = array_literal()
  expected = np.array([33, 27])
  print "Expected [33, 27], got: ", a
  assert np.all(a == expected)
  
if __name__ == '__main__':
  test_idx()
  test_array_literal()
