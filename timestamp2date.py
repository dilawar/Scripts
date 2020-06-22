#!/usr/bin/env python
import sys
import datetime

ts = sys.argv[1].strip()

# If length is 13, it is a js timestamp.
if len(ts) == 13:
    ts = ts[:10]

assert len(ts) == 10, "Timestamp must be 10 char long"
ts = int(ts)
dt = datetime.datetime.fromtimestamp(ts)
print(dt)
