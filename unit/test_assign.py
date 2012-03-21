#!/usr/bin/python
import parakeet
from parakeet import PAR

@PAR
def ret_three():
  x = 3
  return x

def test_assign():
  x = ret_three()
  print "Expected 3, got", x
  assert 3 == x

if __name__ == '__main__':
  test_assign()

