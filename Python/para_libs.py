# Add fixed arg to scan and reduce
# pseudo-python with simpler array syntax
# map(sqrt, [1 4 9]) == [1 2 3]
# map(sum, [[1 2 3], [4 5 6]], axis = 0) == [5 7 9]
# map(sum, [[1 2 3], [4 5 6]], axis = 1) == [6 15]

class ShapeError(Exception):
  pass

import numpy as np

def map(function, *args, **kwargs):
  arrays = args[:]
  try:
    axes = kwargs['axis']
  except KeyError:
    axes = []
  r_axes = axes[:]
  r_axes.reverse()
  try:
    fixed = kwargs['fixed']
  except KeyError:
    fixed = []
  if axes:
    split_arrays = splitArrays(*arrays, axis=axes, curr_done = 0)
  else:
    split_arrays = [[array] for array in arrays]
  #array as in the original sense at the top of the function
  flattened_arrays = []
  for array in split_arrays:
    flattened_array = []
    for split_array in array:
      flattened_array.append(split_array.ravel())
    flattened_arrays.append(flattened_array)

  """
  Arrays a, b
  Things I, II
  Elements 1, 2
  [[[aI1,aII1],[bI1,bII1]],[[aI2,aII2],[bI2,bII2]]]
  Loop through each array
    Loop through each thing and flatten it
  Loop through number of elements
    Loop through each array
      Loop through each thing and append
  function_args = [a_arg0...]
  array_args = [arg1...]
  """
  function_args = []
  for elm_i in range(len(flattened_arrays[0][0])):
    array_args = []
    for array in flattened_arrays:
      ind_args = []
      for flattened_array in array:
        #NOTE: I assume that the arguments into the function are a list, since that's the only way to deal with not splitting the array completely. Ask Alex
        ind_args.append(flattened_array[elm_i])
      array_args.append(ind_args)
    function_args.append(array_args)
  anss = []
  for function_arg in function_args:
    fixed_function_arg = fixed[:]
    fixed_function_arg.extend(function_arg)
    ans = function(*fixed_function_arg)
    ans_shape = np.array(ans).shape
    anss.append(ans)
  mod_shape = list(arrays[0].shape)
  for axis in r_axes:
    del mod_shape[axis]
  mod_shape.extend(ans_shape)
  anss = np.array(anss)
  anss = anss.reshape(mod_shape)
  return anss

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
    if not new_shape:
      new_shape = 1
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

"""
def reduce(function, *args, **kwargs):
  arrays = args[:]
  try:
    axes = range(arrays[0].ndim)
    kwargs['axis'].reverse()
    for selection in kwargs['axis']:
      del axes[selection]
  except KeyError:
    axes = []
  split_arrays = []
  shapes = []
  for array in arrays:
    array_split = [array]
    shape = list(array.shape)
    shapes.append(shape)
    for axis in axes:
      if type(array_split) == list:
        new_split = []
        for array in array_split:
          new_split.extend(np.array_split(array, shape[axis], axis=axis))
        array_split = new_split
      else:
        array_split = np.array_split(array, shape[axis], axis=axis)
    split_arrays.append(array_split)
  result = []

  fin_shape = ()
  axes.sort()
  axes.reverse()
  for axis in axes:
    for shape in shapes:
      del shape[axis]
      if not shape:
        shape.append(1)
  for index, array_split in enumerate(split_arrays):
    for arr_index, array in enumerate(array_split):
      array_split[arr_index] = array.reshape(shapes[index])
  #print "SA",split_arrays, len(split_arrays[0][0])
  for index in range(len(split_arrays[0])):
    #Do something for the first argument
    #for j_index in range(len(split_arrays[0][0])):
    curr_val = []
    for array_split in split_arrays:
      curr_val.append(array_split[index][0])
    for j_index in range(1,len(split_arrays[0][0])):
      #Special case, curr_val is initialized with one extra dimension
      #In case it should have multiple inputs already
      if j_index > 1:
        f_args = [curr_val]
      else:
        f_args = curr_val
      for array_split in split_arrays:
        f_args.append(array_split[index][j_index])
#      print "f_args",f_args
      curr_val = function(*f_args)
#      print "curr_val",curr_val
      ####array_split[0][j_index] = curr_val
      if (fin_shape == ()):
        if not np.isscalar(curr_val):
          fin_shape = curr_val.shape
      else:
        if fin_shape != curr_val.shape:
          raise ShapeError()
    result.append(curr_val)
  np_result = np.array(result)
  np_result.reshape(list(fin_shape).extend(shapes[0]))
  return np_result


def scan(function, *args, **kwargs):
  arrays = args[:]
  try:
    axes = kwargs['axis']
  except KeyError:
    axes = range(arrays[0].ndim)
  split_arrays = []
  shapes = []
  for array in arrays:
    array_split = array
    shape = list(array.shape)
    shapes.append(shape)
    for axis in axes:
      if type(array_split) == list:
        new_split = []
        for array in array_split:
          new_split.extend(np.array_split(array, shape[axis], axis=axis))
        array_split = new_split
      else:
        array_split = np.array_split(array, shape[axis], axis=axis)
    split_arrays.append(array_split)
  result = []

  fin_shape = ()
  axes.sort()
  axes.reverse()
  for axis in axes:
    for shape in shapes:
      del shape[axis]
      if not shape:
        shape.append(1)
  for index, array_split in enumerate(split_arrays):
    for arr_index, array in enumerate(array_split):
      array_split[arr_index] = array.reshape(shapes[index])
  for index in range(len(split_arrays[0])):
    #Do something for the first argument
    curr_val = {}
    for j_index in range(len(split_arrays[0][0])):
      f_args = []
      for array_split in split_arrays:
        f_args.append(array_split[index][j_index])
      curr_val = {"init":function(*f_args,**curr_val)}
      split_arrays[0][index][j_index] = curr_val["init"]
      if (fin_shape == ()):
        if not np.isscalar(curr_val["init"]):
          fin_shape = curr_val["init"].shape
      else:
        if fin_shape != curr_val["init"].shape:
          raise ShapeError()

  np_result = np.array(split_arrays[0])
  np_result.reshape(list(fin_shape).extend(shapes[0]))
  return np_result
"""
def add(x,y):
  return y[0] + x[0]


if __name__ == "__main__":
  import math
  print map(np.sum, np.array([[1,2,3],[4,5,6]]), axis=[0])
  print map(np.sum, np.array([[1,2,3],[4,5,6]]), axis=[1])
  print map(np.sum, np.array([[1,2,3],[4,5,6]]), axis=[0,1])
  print map(np.sum, np.array([[1,2,3],[4,5,6]]))
  print map(add, np.array([[1,2,3],[4,5,6]]),np.array([[1,2,3],[4,5,6]]))
  print map(math.sqrt, np.array([1,4,9,16,25]))


  #print map(add, np.array([[1,2,3],[4,5,6]]),np.array([[1,3,5],[2,4,6]]),
  #          np.array([[1,2,4],[3,5,6]]))

#  def sum(x):
#    return reduce(add, x)

#  print reduce(add, np.array([[1,2,3],[4,5,6]]), axis = [0])
#  print reduce(add, np.array([[1,2,3],[4,5,6]]), axis = [1])
#  print sum(np.array([[1,2,3],[4,5,6]]))
  #[ 6 15]
  #[5 7 9]
#  print scan(add, np.array([[1,2,3],[4,5,6]]), axis = [0])
#  print scan(add, np.array([[1,2,3],[4,5,6]]), axis = [1])