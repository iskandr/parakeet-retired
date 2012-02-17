import numpy as np
import parakeet
from parakeet import PAR 

@PAR
def sum(x):
  return parakeet.reduce(parakeet.add, x)

def test_sum():
  x = np.random.randint(0,100,1000)
  parakeet_sum = sum(x)
  np_sum = np.sum(x)
  print "Python: %s, Parakeet: %s" % (python_sum, parakeet_sum)
  assert parakeet_sum == np_sum 

if __name__ == '__main__':
  test_sum()
