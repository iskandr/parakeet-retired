#!/usr/bin/python
import numpy as np
import parakeet
from parakeet import PAR 

np.random.seed(0)

def add2(x, y):
  return x + y

@PAR
def sum(x):
  return parakeet.reduce(add2, x)

def test_sum():
  x = np.ones((1000,50), dtype=np.float)
  parakeet_sum = sum(x)
  np_sum = np.sum(x)
  for i in range(8):
    print "Python sum %d: %d" % (i, np.sum(x[i*125:(i+1)*125]))
  print "Python: %s, Parakeet: %s" % (np_sum, parakeet_sum)
  assert np_sum == np_sum 

if __name__ == '__main__':
  test_sum()

