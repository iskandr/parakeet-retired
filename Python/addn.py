from parakeet import GPU
from numpy import *

@GPU
def mult9(x):
  return x * 9

@GPU
def addxy(x,y):
  return x * y + x

x = array([1,2,3,4,5,6,7,8,9,10,11,12,13,14])
y = array([3,1,4,1,5,9,3,1,4,1,5,9,3,1])
#print mult9(x)
print addxy(x,y)