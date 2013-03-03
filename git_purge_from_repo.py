#!/usr/bin/env python
import os
import subprocess
import sys 
import string
 
usage = "Usage : purge_git_history.py [-f | -d] file_or_dir_name"
if len(sys.argv) < 3 :
  print usage
  sys.exit(0)

if sys.argv[1] == "-f" :
  filename = sys.argv[2]
  print("Purging file {0} ...".format(filename))
  command = '''git filter-branch -f --index-filter 'git rm -f {0}' 
    --tag-name-filter cat -- --all'''.format(filename)
  command = string.replace(command, "\n", " ")
  subprocess.call(command, shell=True)

elif sys.argv[1] == "-d" :
  dirname = sys.argv[2]
  command = '''git filter-branch -f --tree-filter 'rm -rf {0}' 
  --tag-name-filter cat -- --all'''.format(dirname)
  command = string.replace(command, "\n", " ")
  print("Purging directory {0}".format(dirname))
  subprocess.call(command, shell=True)

else :
  print("Invalid option.")
  print(usage)
  sys.exit()

## Purge the local references.
subprocess.call("rm -rf ./.git/refs/original", shell=True)
subprocess.call("git reflog expire --expire=now --all", shell=True)
subprocess.call("git gc --prune=now", shell=True)
subprocess.call("git push origin master --force", shell=True)

