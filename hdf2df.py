#!/usr/bin/env python3
import pandas as pd
import sys

fpath = sys.argv[1]

if len(sys.argv)>2:
    key = sys.argv[2]
    df = pd.read_hdf(fpath, key=key)
else:
    df = pd.read_hdf(fpath)

print(df)
