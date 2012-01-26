# Add fixed arg to scan and reduce
# pseudo-python with simpler array syntax
# map(sqrt, [1 4 9]) == [1 2 3]
# map(sum, [[1 2 3], [4 5 6]], axis = 0) == [6 15]
# map(sum, [[1 2 3], [4 5 6]], axis = 1) == [5 7 9]

class ShapeError(Exception):
  pass

import numpy as np

def map(function, *args, **kwargs):
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
  try:
    fixed = kwargs['fixed']
  except KeyError:
    fixed = []
  for index in range(len(split_arrays[0])):
    args = fixed[:]
    for array_index, array_split in enumerate(split_arrays):
      args.append(array_split[index].reshape(shapes[array_index]))
    ans = function(*args)
    if (fin_shape == ()):
      if not np.isscalar(ans):
        fin_shape = ans.shape
    else:
      if fin_shape != ans.shape:
        raise ShapeError()
    result.append(ans)
  np_result = np.array(result)
  np_result.reshape(list(fin_shape).extend(shapes[0]))
  return np_result

def reduce(function, *args, **kwargs):
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
      curr_val = {"init": function(*f_args, **curr_val)}
      ####array_split[0][j_index] = curr_val
      if (fin_shape == ()):
        if not np.isscalar(curr_val["init"]):
          fin_shape = curr_val["init"].shape
      else:
        if fin_shape != curr_val["init"].shape:
          raise ShapeError()
    result.append(curr_val["init"])
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


if __name__ == "__main__":
  import math
  print map(math.sqrt, np.array([1,4,9,16,25]), axis=[0])
  print map(sum, np.array([[1,2,3],[4,5,6]]), axis=[0])
  print map(sum, np.array([[1,2,3],[4,5,6]]), axis=[1])
  print map(sum, np.array([[1,2,3],[4,5,6]]), axis=[0,1])

  def add(x,init=0):
    return init + x

  #print map(add, np.array([[1,2,3],[4,5,6]]),np.array([[1,3,5],[2,4,6]]),
  #          np.array([[1,2,4],[3,5,6]]))

  print reduce(add, np.array([[1,2,3],[4,5,6]]), axis = [0])
  print reduce(add, np.array([[1,2,3],[4,5,6]]), axis = [1])
  print scan(add, np.array([[1,2,3],[4,5,6]]), axis = [0])
  print scan(add, np.array([[1,2,3],[4,5,6]]), axis = [1])