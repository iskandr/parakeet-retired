import parakeet
from parakeet import PAR
import numpy as np 



@PAR
def scalar_tuple():
  return 2, 3.5 

def test_scalar_tuple():
  t = scalar_tuple()
  t2 = scalar_tuple.call_original()
  print "Python: %s, Parakeet: %s" % (t2, t)
  assert t == t2

@PAR 
def array_tuple():
  x = np.array([1,2,3])
  y = np.array([1.0, 2.0, 3.0])
  return x, y
 

def test_array_tuple():
  t1 = array_tuple()
  t2 = array_tuple.call_original()
  print "Python: %s, Parakeet: %s" % (t2, t1)
  assert len(t1) == len(t2)
  assert np.all(t1[0] == t2[0])
  assert np.all(t1[1] == t2[1])
  

@PAR
def if_return(b):
  x = -1 
  if b:
    return 1
  return x 
    

def test_conditional():
  a = if_return(True)
  print "Python = 1, Parakeet = ", a
  assert a == 1
  b = if_return(False)
  print "Python = -1, Parakeet = ", b
  assert b == -1
  
if __name__ == '__main__':
  test_scalar_tuple()
  test_array_tuple()
  test_conditional()
