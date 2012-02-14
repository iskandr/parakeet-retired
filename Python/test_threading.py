#!/usr/bin/python
import numpy as np
from parakeet import PAR

@PAR
def times3(X):
  return X * 3

def test_times():
  X = np.array([2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17])
  o = times3(X)
  r = times3.call_original(X)
  print "Got: ", o
  print "Expected: ", r
  assert np.all(o == r)

if __name__ == '__main__':
  test_times()
  test_times()
  test_times()

