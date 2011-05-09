from numpy import *  
from parakeet import GPU
import test_indexing
from test_scalars import f as ff

@GPU
def addxy(x,y):
  return x + y

@GPU
def mult3fcall(x):
  mult = addxy(1,2)
  return x * mult

@GPU
def module_function_calls(x):
  return test_indexing.test_multidiminput.sum_rows(x)

@GPU
def from_as_call(x):
  return ff(x,2)


arrtest = array([1,2,3,4,5,6,7,8,9,10],dtype = int32)
scaltest = 8

import unittest

class MyTest(unittest.TestCase):

    def testMethod(self):
        self.assertTrue(all(mult3fcall(arrtest) == arrtest*3), "mult3fcall(arrtest) != 3*arrtest")
        self.assertTrue(mult3fcall(scaltest)==24,"mult3fcall(scaltest) != 24")
        self.assertTrue(from_as_call(scaltest)==9,"from_as_call failed")
        self.assertTrue(module_function_calls(arrtest)==55,"module_function_calls failed")

if __name__ == '__main__':
    unittest.main()
    
