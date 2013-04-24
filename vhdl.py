#!/usr/bin/env python
import sys 
import re
import subprocess

file = sys.argv[1]
pattern = re.compile(r"entity\s+(?P<name>\w+)\s+is"
    , re.IGNORECASE | re.DOTALL)

text = open(file, "r").read()

m = pattern.search(text)
if m :
  entityName = m.group('name')
  print("[INFO] Found a top most entity : {0}".format(entityName))
  subprocess.call("ghdl -i {0}".format(file), shell=True)
  subprocess.call("ghdl -m {0} --vcd=out.vcd --stop-time=1ms".format(entityName), shell=True)
else :
  print("File does not contain any entity");
  sys.exit(0)


