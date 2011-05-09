from numpy import *  
from parakeet import GPU

@GPU
def sum_rows(x):
  return sum(x)

#@GPU
#def mult3(x):
#  return x * 3

#@GPU
#def basic_return(x):
#  map(mult3)

arr_test = array([1,2,3,4,5,6,7,8,9,10],dtype = int32)
#arr_index = array([3,5,7],dtype = int32)
#arr_index_ans = array([4,6,8],dtype = int32)
#arr_a = array([0,1,3,1,2,1,3,3,5,6],dtype = int32)
#i = 1
#scal_test = 8
multi_test = array([[1,2,3],[4,5,6],[7,8,9]],dtype = int32)
multi_ans = sum(multi_test,1)

#print multi_test
#print sum_rows(multi_test)
#print multi_ans
#print basic_return(multi_test)

import unittest

class MyTest(unittest.TestCase):

  def testMethod(self):
    self.assertTrue(all(sum_rows(multi_test) == multi_ans),"sum_rows not working")
if __name__ == '__main__':
  unittest.main()