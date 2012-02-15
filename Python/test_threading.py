#!/usr/bin/python
import numpy as np
from parakeet import PAR
import timeit

X = np.random.random([16*1000])
 
@PAR
def times3(X):
  return X * 3

def test_times():
  s = "from __main__ import times3,X"
  t = timeit.Timer(setup=s, stmt='times3(X)')
  print "Parakeet time: ", t.timeit(number=1)
  t = timeit.Timer(setup=s, stmt='times3.call_original(X)')
  print "Python time: ", t.timeit(number=1)

if __name__ == '__main__':
  test_times()
  #test_times()
  #test_times()

