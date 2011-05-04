from parakeet import GPU
from numpy import *

@GPU
def mult9(x):
#  y = mean(x)
#  x = x * 2
#  x = x + 4
  return x * 9

@GPU
def addxy(x,y):
  z = mult9(x)
  return x + z 
#@GPU
def whiletest(x):
  a = False
  while not a:
#    a = False
    a = True
  return x

#@GPU
def sumtest(x):
  return sum(x)

x = array([1,2,3,4,5,6,7,8,9,10,11,12,13,14],dtype = int32)
y = array([3,1,4,1,5,9,3,1,4,1,5,9,3,1],dtype = int32)
z = array([[1,2,3],[4,5,6],[7,8,9]],dtype = int32)
f = array([1.2, 2.5,3.4,1.2,5.3,1.5,9,8,7,6,1,2,3],dtype = float32)
#print mult9(x)
print addxy(x,y)
#print whiletest(x)
#print sumtest(z)
