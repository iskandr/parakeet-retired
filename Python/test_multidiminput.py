from numpy import *  
from parakeet import GPU

@GPU
def sum_rows(x):
  return sum(x)

@GPU
def calc_centroid(X,a,i):
    return mean(X[a == i])

#@GPU
#def mult_output(X,assign,k):
#  C = map(partial(calc_centroid, X, assign), arange(k))
#  return C



arr_test = array([1,2,3,4,5,6,7,8,9,10],dtype = int32)
multi_test = array([[1,2,3],[4,5,6],[7,8,9]],dtype = int32)
multi_ans = sum(multi_test,1)

X = array([[1,2,3],[4,5,6]],dtype = int32)
assign = array([0,1],dtype = int32)
k = 2
#mult_output(X,assign,k)

import unittest

class MyTest(unittest.TestCase):

  def testMethod(self):
    self.assertTrue(all(sum_rows(multi_test) == multi_ans),"sum_rows not working")
#if __name__ == '__main__':
#  unittest.main()