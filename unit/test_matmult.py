import numpy as np 
import parakeet 

@parakeet.PAR
def dot(x,y):
  return parakeet.reduce(np.add, x*y)

# for now, assume Y is already transposed 
@PAR
def mm(X,Y):
  return allpairs(dot, X, Y, axis=[0])

def test_mm():
  X = np.random.randn(1000, 1000)
  Y = np.random.randn(1000, 1000)
  parakeet_result = mm(X,Y.T)
  np_result = np.dot(X,y)
  assert np.sum( (parakeet_result - np_result) ** 2) <= 0.0001

if __name__ == '__main__':
  test_mm()
  
  
