#!/usr/bin/python
import numpy as np
import parakeet, sys
from parakeet import PAR

parakeet.set_vectorize(False)

@PAR 
def implicit_map(x):
  return x * 3

def test_implicit_map():
  print "Testing implicit maps" 
  x = np.array([range(10),
                range(10,20),
                range(20,30),
                range(30,40),
                range(40,50),
                range(50,60),
                range(60,70),
                range(70,80),
                range(80,90),
                range(90,100)], dtype=np.int32)
  y = implicit_map(x.T)
  y_original = implicit_map.call_original(x.T)
  print "Python = %s\n Parakeet = %s" % (y_original, y)
  assert np.all(y_original == y)

if __name__ == '__main__':
  test_implicit_map()
  test_implicit_map()

