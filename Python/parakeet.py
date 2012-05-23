from parakeet_ast import register_function 
from parakeet_wrapped_function import WrappedFunction 

def PAR(old_function):
  untyped_id, global_vars = register_function(old_function)
  return WrappedFunction(old_function, untyped_id, global_vars)

## Adverbs
## These rely on the methods of Parakeet's WrappedFunction

def map(fn, *args, **kwargs):
  if not isinstance(fn, WrappedFunction):
    fn = PAR(fn)
  return fn.map(*arg, **kwargs)

def reduce(fn, *args, **kwargs):
  if not isinstance(fn, WrappedFunction):
    fn = PAR(fn)
  return fn.reduce(*args, **kwargs)

def scan(fn, *args, **kwargs):
  if not isinstance(fn, WrappedFunction):
    fn = PAR(fn)
  return fn.scan(*args, **kwargs)

def allpairs(fn, *args, **kwargs):
  if not isinstance(fn, WrappedFunction):
    fn = PAR(fn)
  return fn.allpairs(*args, **kwargs)

## Library functions
def abs(x):
  if x < 0:
    x = -1*x
  return x

def _argmin_helper(currMin, currMinIndex, currIndex, currArrVal):
  if currArrVal < currMin:
    currMin = currArrVal
    currMinIndex = currIndex
  return currMin, currMinIndex, currIndex+1

def argmin(x):
  gib1,res,gib2 = reduce(_argmin_helper, x, default=[100000000000,-1,0])
  return res

def _and(x,y):
  return x and y

def _or(x,y):
  return x or y 

def all(x):
  return reduce(_and, x, default=True)

def any(x):
  return reduce(_or, x, default=False)

def add(x,y):
  return x+y

def sub(x,y):
  return x-y

def mult(x,y):
  return x*y

def div(x,y):
  return x/y

def sum(x, axis = None):
  return reduce(add, x, axis=axis)

def prod(x, axis = None):
  return reduce(mult, x, axis=axis)

def mean(x, axis = None):
  total = 1. * sum(x, axis = axis)
  return total / len(x)

def dot(x,y):
  return sum(x*y)

def _len(x):
  return np.size(x, 0)

from parakeet_configuration import *



