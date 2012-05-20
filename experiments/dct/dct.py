#!/usr/bin/env python
import numpy
from numpy import pi,zeros
from math import cos,sqrt
from PIL import Image
from ColorSpace import ColorSpace

import numpy as np 
from scipy.fftpack import dct 

class DCT(object):
  # Example use:
  #   from dct import DCT
  #   dct = DCT('test.jpg')
  #   ret = dct.get_dcts()
  def __init__(self, image_path):
    self.image_path = image_path
    self.rgb_image = None
    self.rgb_array = None
    self.ycc_array = None
    self.ycc_sub_images = None
    self.cos_dict = None

  def _decompose_ycc(self, window_width = 8, window_height = 8):
    # Imperfectly grabs all of the 8x8 pixel blocks (will ignore edge blocks
    # that are smaller than 8x8).
    
    width, height,_ = self.ycc_array.shape

    width_intevals = width / window_width
    height_intervals = height / window_height

    sub_images = []
    im = self.ycc_array
    for height_interval in range(width_intevals):
      height_start = height_interval * window_height
      height_end = height_start + window_height
      for width_interval in range(height_intervals):
        width_start = width_interval * window_width
        width_end = width_start + window_width 
        sub_image = im[height_start:height_end, width_start:width_end, :]
        sub_images.append(sub_image)
    return sub_images

  def get_dcts(self):
    if self.rgb_image is None:
      self.rgb_image = Image.open(self.image_path)
      self.rgb_array = np.asarray(self.rgb_image)

    if self.ycc_array is None:
      color_space = ColorSpace()
      red = self.rgb_array[:,:,0]
      green = self.rgb_array[:,:,1]
      blue = self.rgb_array[:,:,2]
      lum, cb, cr = color_space.to_ycc(red, green, blue)
      x,y = lum.shape
      self.ycc_array = np.zeros( (x,y,3), dtype=lum.dtype)
      self.ycc_array[:,:,0] = lum
      self.ycc_array[:,:,1] = cb
      self.ycc_array[:,:,2] = cr
        
    if self.ycc_sub_images is None:
      self.ycc_sub_images = self._decompose_ycc()

    ret_dcts = []
    print "Num sub-images:", len(self.ycc_sub_images)
    for window in self.ycc_sub_images:
      lum, _, _ = window[:, :, 0], window[:,:,1], window[:,:,2]
         
      # switched to scipy dct for performance 
      #lum_dct = dct(lum, type=2, norm='ortho' )
      lum_dct = self.two_dim_DCT(lum)
        
    
      # TODO(tierney): Technically, need to subsample Cb and Cr before
      # DCT. These Cb and Cr values should be treated as unrealistic until
      # subsampling before the DCT step.
      #cb_dct = two_dim_DCT(_cb_pixels)
      #cr_dct = two_dim_DCT(_cr_pixels)
      ret_dcts.append(lum_dct)
    return numpy.array(ret_dcts)

  def two_dim_DCT(self, X):
    """2D Discrete Cosine Transform
    X should be square 2 dimensional array
    Trying to follow:

    http://en.wikipedia.org/wiki/Discrete_cosine_transform#Multidimensional_DCTs
    http://en.wikipedia.org/wiki/JPEG#Discrete_cosine_transform"""

    result = zeros(X.shape)
    N1,N2 = X.shape
    
    if self.cos_dict == None:
      self.cos_dict = [[cos((pi/8)*(x+0.5)*y) \
                        for y in range(N1)] for x in range(N1)]

    def alpha(n):
      if n == 0:
        return 0.353553390593 #sqrt(1/8.)
      else:
        return .5 #sqrt(2/8.)

    for (k1,k2), _ in numpy.ndenumerate(X):
      sub_result = 0.
      for n1 in range(N1):
        for n2 in range(N2):
          sub_result += X[n1,n2] * self.cos_dict[n1][k1] * self.cos_dict[n2][k2]
      result[k1,k2] = alpha(k1) * alpha(k2) * sub_result
    return result
