from numpy import *  
from parakeet import PAR

@PAR
def assignConst(x):
  x[2] = 8
  return 0

def test_assign_const():
  arr_test = array([1,2,3,4,5,6,7,8,9,10], dtype = int64)
  print "Testing assigning to a constant index"
  print "Input: ", arr_test
  assignConst(arr_test)
  print "Expected %d, got %d" % (arr_test[2], 8)
  assert 8 == arr_test[2]

if __name__ == '__main__':
  test_assign_const()

