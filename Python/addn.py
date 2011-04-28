from PythonASTs import GPU
from numpy import *

@GPU
def add2(x):
  return x * 9

x = array([1,2,3,4,5,6,7,8,9,10,11,12,13,14])
print add2(x)


