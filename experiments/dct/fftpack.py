import numpy as np
from scipy import fftpack

g = np.array([[-76, -73, -67, -62, -58, -67, -64, -55],
              [-65, -69, -73, -38, -19, -43, -59, -56],
              [-66, -69, -60, -15,  16, -24, -62, -55],
              [-65, -70, -57,  -6,  26, -22, -58, -59],
              [-61, -67, -60, -24,  -2, -40, -60, -58],
              [-49, -63, -68, -58, -51, -60, -70, -53],
              [-43, -57, -64, -69, -73, -67, -63, -45],
              [-41, -49, -59, -60, -63, -52, -50, -34]], dtype=np.float32)

h = np.array([[48, 39, 40, 68, 60, 38, 50, 121],
              [149, 82, 79, 101, 113, 106, 27, 62],
              [58, 63, 77, 69, 124, 107, 74, 125],
              [80, 97, 74, 54, 59, 71, 91, 66],
              [18, 34, 33, 46, 64, 61, 32, 37],
              [149, 108, 80, 106, 116, 61, 73, 92],
              [211, 233, 159, 88, 107, 158, 161, 109],
              [212, 104, 40, 44, 71, 136, 113, 66]])

fg = fftpack.dct(g-0.0, type=2, norm='ortho')
fh = fftpack.dct(h-128.0, type=2, norm='ortho')