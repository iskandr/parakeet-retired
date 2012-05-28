import parakeet_wrapped_function

# Note: not using any names from parakeet_wrapped_function within top-level
# statements to avoid recursive dependency between module imports 

## Adverbs
## These rely on the methods of Parakeet's WrappedFunction

def map(fn, *args, **kwargs):
  if not isinstance(fn, parakeet_wrapped_function.WrappedFunction):
    fn = parakeet_wrapped_function.PAR(fn)
  return fn.map(*args, **kwargs)

def reduce(fn, *args, **kwargs):
  if not isinstance(fn, parakeet_wrapped_function.WrappedFunction):
    fn = parakeet_wrapped_function.PAR(fn)
  return fn.reduce(*args, **kwargs)

def scan(fn, *args, **kwargs):
  if not isinstance(fn, parakeet_wrapped_function.WrappedFunction):
    fn = parakeet_wrapped_function.PAR(fn)
  return fn.scan(*args, **kwargs)

def allpairs(fn, *args, **kwargs):
  if not isinstance(fn, parakeet_wrapped_function.WrappedFunction):
    fn = parakeet_wrapped_function.PAR(fn)
  return fn.allpairs(*args, **kwargs)
