#!/usr/bin/env python
import os
import sys 
import string
 
usage = "Usage : purge_git_history.py [-f | -d] file_or_dir_name"
if len(sys.argv) < 3 :
  print usage
  sys.exit(0)

if sys.argv[1] == "-f" :
  filename = sys.argv[2]
  command = '''git filter-branch -f --index-filter 'git rm --cached
  --ignore-unmatch {0}' --prune-empty --tag-name-filter cat --
  --all'''.format(filename)
  command = string.replace(command, "\n", " ")
  print("Purging file {0} ...".format(filenae))
  print(command)
  os.system(command)

elif sys.argv[1] == "-d" :
  dirname = sys.argv[2]
  command = '''git filter-branch -f --tree-filter 'rm -rf {0}' 
  --tag-name-filter cat -- --all'''.format(dirname)
  command = string.replace(command, "\n", " ")
  print("Purging directory {0}".format(dirname))
  sys.command(command)

else :
  print("Invalid options.")
  print(usage)
  sys.exit()

## Purge the local references.
sys.command("rm -rf ./.git/refs/original")
sys.command("git reflog expire --expire=now --all")
sys.command("git gc --prune=now")
sys.command("git push origin master --force)

