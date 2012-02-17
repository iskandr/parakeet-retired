import numpy as np
import time

def eq(a, b):
  if a.dtype is np.dtype('float64'):
    return abs(a - b) < 0.0001
  else:
    return a == b

def run(p_func, np_func, args):
  t1 = time.time()
  p = p_func(*args)
  t2 = time.time()
  ptime = (t2 - t1)*1000.0
  t1 = time.time()
  n = np_func(*args)
  t2 = time.time()
  ntime = (t2 - t1)*1000.0
  assert np.all(eq(p, n))
  return (ptime, ntime)

def print_result(s, times):
  print "Parakeet time on %s: %fms" % (s, times[0])
  print "NumPy time on %s: %fms" % (s, times[1])
  print "Parakeet speedup over Numpy on %s: %fms (%.2f%%)" % \
    (s, times[1] - times[0], 100 * (times[1] - times[0]) / times[0])

