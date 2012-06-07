import parakeet as par
import numpy as np 

def test_mean():
  x = np.random.randn(1000) 
  true_mean = np.mean(x)
  parakeet_mean = par.mean(x)
  print "Numpy mean = %s, Parakeet mean = %s" % (true_mean, parakeet_mean)
  assert abs(true_mean - parakeet_mean) < 0.00001

if __name__ == '__main__':
  test_mean()
