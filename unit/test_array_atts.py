#!/usr/bin/python
import parakeet
from parakeet import PAR
import numpy as np

@PAR
def transpose(x):
  return x.transpose()

def test_transpose():
  x = np.array([[1,2],[3,4]])
  e_out = np.array([[1,3],[2,4]])
  out = transpose(x)
  print "Expected ", e_out, " got: ", out
  assert np.all(out == e_out)

if __name__ == '__main__':
  test_transpose()