#!/usr/bin/python
import numpy as np
import parakeet, sys
from parakeet import PAR

@PAR 
def implicit_map(x):
  return x * 3

def test_implicit_map():
  print "Testing implicit maps" 
  x = np.array(range(100), dtype=np.int32)
  y = implicit_map(x)
  y_original = implicit_map.call_original(x)
  print "Python = %s, Parakeet = %s" % (y_original, y)
  assert np.all(y_original == y)

if __name__ == '__main__':
  test_implicit_map()

