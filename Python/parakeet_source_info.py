from ctypes import * 

###############################################################################
#  C types struct for source information
###############################################################################

class _c_source_info_t(Structure):
  _fields_ = [("filename", c_char_p),
              ("line", c_int),
              ("col", c_int)]

class _source_info_t:
  def __init__(self, c_src):
    self.c_source_info = c_src
    if c_src is not None:
      self.addr = addressof(c_src)
    else:
      self.addr = 0


def src_addr(src_info):
  if src_info is None: 
    return None
  else: 
    return src_info.addr
