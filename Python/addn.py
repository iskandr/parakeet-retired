from parakeet import GPU
from numpy import *

@GPU
def mult9(x):
#  y = x + 2
  x = x + 1 
  return x * 9 + 32 - x

#@GPU
def addxy(x,y,z):
  return x * y + z
#@GPU
def whiletest(x):
  a = False
  while not a:
#    a = False
    a = True
  return x

x = array([1,2,3,4,5,6,7,8,9,10,11,12,13,14],dtype = int32)
y = array([3,1,4,1,5,9,3,1,4,1,5,9,3,1],dtype = int32)
z = array([[1,2,3],[4,5,6]],dtype = int32)
print mult9(x)
#print addxy(x,y,x)
#print whiletest(x)