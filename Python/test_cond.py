from numpy import *
from parakeet import PAR
import sys

@PAR 
def simple_if(x):
  a = 0
  if x:
    a = 1
    a = 2
  else:
    a = 0
    a = 3
  return a

def test_if():
  print "Testing If"
  sys.stdout.flush()
  x = simple_if(1)
  print "Expected 1, got", x
  assert 1 == x
  y = simple_if(0)
  print "Expected 0, got", y
  assert 0 == y

if __name__ == '__main__':
  test_if()

