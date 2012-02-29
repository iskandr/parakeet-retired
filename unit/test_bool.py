#!/usr/bin/python
from numpy import *
import parakeet, sys
from parakeet import PAR

@PAR
def bool(c):
  if c:
    return 6.0
  else:
    return 3.0

def test_bool():
  print "Testing boolean arguments"
  sys.stdout.flush()
  x = bool(True)
  print "Expected 6.0, got", x 
  assert 6.0 == x

if __name__ == '__main__':
  test_bool()

