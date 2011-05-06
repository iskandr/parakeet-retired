from numpy import *  
from parakeet import GPU

@GPU
def addxy(x,y):
  return x + y

@GPU
def mult3fcall(x):
  mult = addxy(1,2)
  return x * mult

arrtest = array([1,2,3,4,5,6,7,8,9,10],dtype = int32)
scaltest = 8

print arrtest == arrtest
import unittest

class MyTest(unittest.TestCase):

    def testMethod(self):
        self.assertTrue(all(mult3fcall(arrtest) == arrtest*3), "mult3fcall(arrtest) != 3*arrtest")
        self.assertTrue(mult3fcall(scaltest)==24,"mult3fcall(scaltest) != 24")

if __name__ == '__main__':
    unittest.main()