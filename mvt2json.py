#!/usr/bin/env python3

__author__ = "Dilawar Singh"
__email__ = "dilawar@subcom.tech"

import mapbox_vector_tile as mvt
import sys
from pathlib import Path

infile = Path(sys.argv[1])
assert infile.exists(), f"{infile} does not exists."

with open(infile, 'rb') as  f:
    buf = f.read()
    print(mvt.decode(buf))
