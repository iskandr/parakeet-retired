from collections import Iterable 

from parakeet_ast import register_function, AutoTranslate 
from parakeet_wrapped_function import WrappedFunction 

def PAR(old_function):
  untyped_id, global_vars = register_function(old_function)
  return WrappedFunction(old_function, untyped_id, global_vars)


def parakeet_prim(*lib_fns):
  # we might be auto-translating multiple library functions, 
  # make a list of the singleton if only one gets passed in 
  def wrap(fn):
    wrapped = PAR(fn)
    for lib_fn in lib_fns:
      AutoTranslate[lib_fn] = wrapped
    return wrapped 
  return wrap 

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
import numpy as np 
@parakeet_prim(abs, np.abs)
def abs(x):
  if x < 0:
    x = -1*x
  return x

def _argmin_helper(currMin, currMinIndex, currIndex, currArrVal):
  if currArrVal < currMin:
    currMin = currArrVal
    currMinIndex = currIndex
  return currMin, currMinIndex, currIndex+1

@parakeet_prim(np.argmin)
def argmin(x):
  gib1,res,gib2 = reduce(_argmin_helper, x, default=(100000000000,-1,0))
  return res

def _and(x,y):
  return x and y

def _or(x,y):
  return x or y 

@parakeet_prim(np.all, all)
def all(x):
  return reduce(_and, x, default=True)

@parakeet_prim(np.any, any)
def any(x):
  return reduce(_or, x, default=False)

@parakeet_prim(np.add)
def add(x,y):
  return x+y


@parakeet_prim(np.sub)
def sub(x,y):
  return x-y


@parakeet_prim(np.mult)
def mult(x,y):
  return x*y


@parakeet_prim(np.div)
def div(x,y):
  return x/y


@parakeet_prim(np.sum)
def sum(x, axis = None):
  return reduce(add, x, axis=axis)


@parakeet_prim(np.prod)
def prod(x, axis = None):
  return reduce(mult, x, axis=axis)


@parakeet_prim(np.mean)
def mean(x, axis = None):
  total = 1. * sum(x, axis = axis)
  return total / len(x)


@parakeet_prim(np.dot)
def dot(x,y):
  return sum(x*y)


@parakeet_prim(len)
def _len(x):
  return np.size(x, 0)

from parakeet_configuration import *



