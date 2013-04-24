#!/usr/bin/env python
import sys 
import re
import subprocess

file = sys.argv[1]
format = file.split("/")[-1]
fmt = format.split(".")[1]


if len(sys.argv) > 2 :
  compiler = "vsim" 
else :
  compiler = "ghdl"

pattern = re.compile(r"entity\s+(?P<name>\w+)\s+is"
    , re.IGNORECASE | re.DOTALL)

text = open(file, "r").read()

m = pattern.search(text)
if m :
  entityName = m.group('name')
  print("[INFO] Found a top most entity : {0}".format(entityName))
  if compiler == "ghdl" :
    subprocess.call("ghdl -i {0}".format(file), shell=True)
    subprocess.call("ghdl -m {0}".format(entityName), shell=True)
    subprocess.call("ghdl -r {0} --vcd={0}.vcd --stop-time=1ms".format(entityName)
        , shell=True)
  elif compiler == "vsim" :
    print("[INFO] Format is {0}".format(fmt))
    subprocess.call("vlib work", shell=True)
    if fmt == "vhd" :
      subprocess.call("vcom *.{0} && vsim -c -do 'run 1ms;quit' {1}".format(fmt, 
        entityName), shell=True)
    elif fmt == "v" :
      subprocess.call("vlog *.{0} && vsim -c -do 'run 1ms;quit' {1}".format(fmt, 
        entityName), shell=True)

else :
  print("File does not contain any entity");
  sys.exit(0)


