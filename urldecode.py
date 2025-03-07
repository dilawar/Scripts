#!/usr/bin/env python3

import sys
from urllib.parse import unquote
print(unquote(sys.stdin.read()))
