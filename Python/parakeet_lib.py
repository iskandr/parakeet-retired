#-----Library functions
def abs(x):
  if x < 0:
    x = -1*x
  return x

def argminHelper(currMin, currMinIndex, currIndex, currArrVal):
  if currArrVal < currMin:
    currMin = currArrVal
    currMinIndex = currIndex
  return currMin, currMinIndex, currIndex+1

def argmin(x):
  gib1,res,gib2 = reduce(argminHelper, x,default=[100000000000,-1,0])
  return res

def And(x,y):
  return x and y

def all(x):
  return reduce(And,x,default=1)

def add(x,y):
  return x+y

def mult(x,y):
  return x*y

def sum(x, axis=None):
  return reduce(add, x, axis=axis, default=0)

def mean(x, axis=None):
  total = sum(x, axis=axis)
  return total*1. / len(x)

def dot(x,y):
  return sum(x*y)

#-----Map/Reduce/Scan

class ShapeError(Exception):
  pass

import numpy as np

def splitArrays(*args, **kwargs):
  arrays = args[:]
  num_arrays = len(arrays)
  try:
    axes = kwargs['axis']
  except KeyError:
    axes = []
  curr_done = kwargs['curr_done']
  curr_axis = axes[0]-curr_done
  len_axis = arrays[0].shape[curr_axis]
  split_arrays = []
  for array in arrays:
    new_shape = list(array.shape)
    del new_shape[curr_axis]
    new_arrs = np.split(array, len_axis, axis=curr_axis)
    for i, arr in enumerate(new_arrs):
      new_arrs[i] = arr.reshape(new_shape)
    split_arrays.append(new_arrs)
  results = []
  if len(axes) > 1:
    for dim_i in range(len_axis):
      new_args = []
      for s_a in split_arrays:
        new_args.append(s_a[dim_i])
      if not results:
        results = splitArrays(*new_args,axis=axes[1:],curr_done=curr_done+1)
      else:
        curr_result = splitArrays(*new_args,axis=axes[1:],curr_done=curr_done+1)
        for arr_i in range(num_arrays):
          results[arr_i].extend(curr_result[arr_i])
  else:
    return split_arrays
  return results

def map(function, *args, **kwargs):
  arrays = args[:]
  try:
    axes = kwargs['axis']
    if type(axes) != list and axes != None:
      axes = [axes]
  except KeyError:
    axes = range(arrays[0].ndim)
  if axes == None:
    axes = range(arrays[0].ndim)
  try:
    fixed = kwargs['fixed']
  except KeyError:
    fixed = []
  if axes:
    split_arrays = splitArrays(*arrays, axis=axes, curr_done = 0)
  else:
    split_arrays = [[array] for array in arrays]
  anss = []
  for chunk_i in range(len(split_arrays[0])):
    curr_args = fixed[:]
    #array as in the original sense at the top of the function
    for array in split_arrays:
      curr_args.append(array[chunk_i])
    ans = function(*curr_args)
    ans_shape = np.array(ans).shape
    anss.append(ans)
  mod_shape = []
  orig_shape = arrays[0].shape
  for axis in axes:
    mod_shape.append(orig_shape[axis])
  mod_shape.extend(ans_shape)
  anss = np.array(anss)
  anss = anss.reshape(mod_shape)
  return anss

def reduce(function, *args, **kwargs):
  arrays = args[:]
  try:
    axes = kwargs['axis']
    if type(axes) != list and axes != None:
      axes = [axes]
  except KeyError:
    axes = range(arrays[0].ndim)
  if axes == None:
    axes = range(arrays[0].ndim)
  try:
    fixed = kwargs['fixed']
  except KeyError:
    fixed = []

  if axes:
    split_arrays = splitArrays(*arrays, axis=axes, curr_done = 0)
  else:
    split_arrays = [[array] for array in arrays]
  #If there is a default, use it
  try:
    prev_res = kwargs['default']
    if type(prev_res) != list:
      prev_res = [prev_res]
    start_index = 0
  #Otherwise, call with the first 2 chunks
  except:
    prev_res = []
    for array in split_arrays:
      prev_res.append(array[0])
    start_index = 1
  for chunk_i in range(start_index, len(split_arrays[0])):
    curr_args = fixed[:]
    curr_args.extend(prev_res)
    #array as in the original sense at the top of the function
    for array in split_arrays:
      curr_args.append(array[chunk_i])
    prev_res = function(*curr_args)
    if type(prev_res) == tuple:
      ans = tuple([np.array(res) for res in prev_res])
      prev_res = list(prev_res)
    else:
      ans = np.array(prev_res)
      prev_res = [prev_res]
  return ans

def allpairs(f, x, y, fixed=None, axes=None):
  assert False

def scan(f, *args, **kwargs):
  assert False

def addMultipleRet(x,bo,st,y):
  return y+x, True, "Fish"

if __name__ == "__main__":
  import math
  print map(np.sum, np.array([[1,2,3],[4,5,6]]), axis=[0])
  print map(np.sum, np.array([[1,2,3],[4,5,6]]), axis=[1])
  print map(np.sum, np.array([[1,2,3],[4,5,6]]), axis=[0,1])
  print map(np.sum, np.array([[1,2,3],[4,5,6]]), axis=[])
  print map(add, np.array([[1,2,3],[4,5,6]]),np.array([[1,2,3],[4,5,6]]), axis = [1])
  print map(math.sqrt, np.array([1,4,9,16,25]))

  print reduce(add, np.array([[1,2,3],[4,5,6]]), axis = [0], default=[0])
  print reduce(add, np.array([[1,2,3],[4,5,6]]), axis = [0])
  print reduce(add, np.array([[1,2,3],[4,5,6]]), axis = [1], default=0)
  print reduce(add, np.array([[1,2,3],[4,5,6]]), axis = [1])
  print reduce(addMultipleRet, np.array([[1,2,3],[4,5,6]]), axis = [0], default = [0,True,"Fish"])
  print reduce(add, np.array([1,2,3]), axis = [0])
