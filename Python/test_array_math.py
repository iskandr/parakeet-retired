import numpy as np 
from parakeet import PAR

@PAR 
def array_add(x,y):
  return x + y

def test_array_add_int():
  x = np.array([1,2,3])
  y = np.array([10, 20, 30])
  expected = x + y 
  res = array_add(x,y)
  print "x + y =", expected, "received", res
  assert np.all(res == expected)

def test_array_float():
  x = np.array([1.0, 2.0, 3.0])
  y = np.array([10.0, 20.0, 30.0])
  expected = x + y
  res = array_add(x,y)
  print "x + y=", expected, "received", res
  assert np.all(res == expected)

if __name__ == '__main__': 
  test_array_add_int()
  test_array_add_float() 
