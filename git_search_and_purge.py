#!/usr/bin/env python
import os
import subprocess
import sys 
import string
import re
 
def getOutput(cmd):
  return os.popen(cmd).read()

if (len(sys.argv) < 3):
  print "usage: %s -s size_in_bytes -e regex" % sys.argv[0]
else:
  maxSize = int(sys.argv[2])

def search_files(size, pat) :
  foundFiles = set()
  revisions = getOutput("git rev-list HEAD").split()
  bigfiles = set()
  for revision in revisions:
    files = getOutput("git ls-tree -zrl %s" % revision).split('\t')
    for file in files:
      if file == "":
        continue
      splitdata = file.split()
      if len(splitdata) < 4 :
        continue
      commit = splitdata[-2]
      if splitdata[-1] == "-":
        continue
      size = int(splitdata[-1])
      path = " ".join(splitdata[0:len(splitdata)-3])
      if (size > maxSize):
        path = path.split("\x00100")
        bigfiles.add("{2}".format(size, commit, path[0]))

  bigfiles = sorted(bigfiles, reverse=True)

  for f in bigfiles:
    filename = f.split()[-1].split("/")[-1]
    filepath = f.split("<>")[-1]
    if(len(sys.argv) > 3) :
      pat = sys.argv[4]
      if(re.match(pat, filename)) :
        foundFiles.add(filepath.strip())
      else : pass
    else :
      foundFiles.add(filepath.strip())
  return foundFiles


def purge_file(filename) :
  print("|- Purging file {0} ...".format(filename))
  command = '''git filter-branch -f --index-filter 
  'git rm --cached --ignore-unmatch {0}'
   --prune-empty --tag-name-filter cat -- --all'''.format(filename)
  command = string.replace(command, "\n", " ")
  subprocess.call(command, shell=True)

if __name__ == "__main__" :
  usage = "Usage : git_search_and_purge.py -s size_in_bytes [-e regex]"
  if len(sys.argv) < 3 or len(sys.argv) > 5 :
    print usage
    sys.exit(0)
  size = sys.argv[2]
  if len(sys.argv) == 5 :
    regex = sys.argv[4]
  else :
    regex = ".*"
  
  files = search_files(size, regex)
  for file in files :
    file = string.replace(file, " ", "\ ")
    purge_file(file)
  ### Purge the local references.
  subprocess.call("rm -rf ./.git/refs/original", shell=True)
  subprocess.call("git reflog expire --expire=now --all", shell=True)
  subprocess.call("git gc --prune=now", shell=True)
  subprocess.call("git push origin master --force", shell=True)

