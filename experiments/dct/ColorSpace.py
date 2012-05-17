class ColorSpace(object):
  @staticmethod
  def to_rgb(lum, cb, cr):
    red = lum + 1.402 * (cr-128)
    green = lum - 0.34414 * (cb - 128) - 0.71414 * (cr - 128)
    blue = lum + 1.772 * (cb - 128)
    return red, green, blue

  @staticmethod
  def to_ycc(red, green, blue):
    lum = 0.299 * red + 0.587 * green + 0.114 * blue
    cb = 128 - (0.168736 * red) - (0.331264 * green) + 0.5 * blue
    cr = 128 + 0.5 * red - 0.418688 * green - 0.081312 * blue
    return lum, cb, cr
