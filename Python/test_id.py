from numpy import *
from parakeet import PAR
import sys

@PAR
def identity(x):
  return x

def test_scalar_id():
  print "Testing scalar Identity"
  sys.stdout.flush()
  x = identity(3.0)
  print "Expected 3.0, got", x 
  assert 3.0 == x

def array_id():
  multi_test = reshape(arange(12, dtype=int64), (3,4))
  print "Input:", multi_test
  print "Input Shape:", shape(multi_test)
  print "Input Type:", multi_test.dtype
  res = identity(multi_test)
  print "Output: ", res
  print "Output Shape:", shape(res)
  print "Output Type:", res.dtype
  assert ndarray.__eq__(res, multi_test).all()

if __name__ == '__main__':
  test_scalar_id()
  test_array_id()

