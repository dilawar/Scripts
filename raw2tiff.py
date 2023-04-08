#!/usr/bin/env python3
import sys
import numpy as np
import tifffile

with open(sys.argv[1], 'rb') as f:
    buf = f.read()

data = np.frombuffer(buf, dtype='f4')
size = (67, 41, 58)
data = data.reshape(size, order='F')
outfile = sys.argv[2] if len(sys.argv) > 2 else f'{sys.argv[1]}.tiff'
tifffile.imwrite(f'{outfile}', data.T)
print(f'[INFO] Wrote to {outfile}')
