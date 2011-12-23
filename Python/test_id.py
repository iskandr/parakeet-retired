from numpy import *
from parakeet import PAR
import sys

@PAR
def identity(x):
  return x

def test_scalar_id():
  print "Testing scalar Identity"
  sys.stdout.flush()
  assert 3.0 == identity(3.0)

def array_id():
  multi_test = reshape(arange(12, dtype=int32), (3,4))
  print "Input:", multi_test
  print "Input Shape:", shape(multi_test)
  print "Input Type:", multi_test.dtype
  res =  identity(multi_test)
  print "Output: ", res
  print "Output Shape:", shape(res)
  print "Output Type:", res.dtype
  assert res == multi_test

if __name__ == '__main__':
    test_scalar_id()
