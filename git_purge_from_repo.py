#!/usr/bin/env python
import os
import subprocess
import sys 
import string
 

usage = "Usage : {0} [-f | -d] file_or_dir_name".format(sys.argv[0])

if len(sys.argv) < 3 :
  print usage
  sys.exit(0)

if sys.argv[1] == "-f" :
  filename = sys.argv[2]
  print("Purging file {0} ...".format(filename))
  command = """git filter-branch -f --index-filter 'git rm --cached
  --ignore-unmatch {0}' --tag-name-filter cat -- --all""".format(filename)
  print("Runing: %s" % command)
  command = string.replace(command, "\n", " ")
  subprocess.call(command, shell=True)

elif sys.argv[1] == "-d" :
  dirname = sys.argv[2]
  command = '''git filter-branch -f --tree-filter 'git rm -rf --ignore-unmatch {0}' 
  --tag-name-filter cat -- --all'''.format(dirname)
  command = string.replace(command, "\n", " ")
  print("Purging directory {0}".format(dirname))
  print("Running %s" % command)
  subprocess.call(command, shell=True)

else :
  print("Invalid option.")
  print(usage)
  sys.exit()

## Purge the local references.
subprocess.call("rm -rf ./.git/refs/original", shell=True)
subprocess.call("git reflog expire --expire=now --all", shell=True)
subprocess.call("git gc --prune=now", shell=True)

