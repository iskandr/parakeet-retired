from parakeet import GPU
from numpy import *

@GPU
def mult9(x):
  y = sum(x)
  x = x * 2
  x = x + 4
  return x * y

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
f = array([1.2, 2.5,3.4,1.2,5.3,1.5,9,8,7,6,1,2,3],dtype = float32)
print mult9(x)
#print addxy(f,f,2.1)
#print whiletest(x)
