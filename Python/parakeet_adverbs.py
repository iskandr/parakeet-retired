from parakeet_wrapped_function import WrappedFunction 
from parakeet_decorators import PAR 
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
