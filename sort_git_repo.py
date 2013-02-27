#!/usr/bin/env python
# This is from stackoverflow answer.
# http://stackoverflow.com/questions/298314/find-files-in-git-repo-over-x-megabytes-that-dont-exist-in-head
# 
import os, sys
import string

def getOutput(cmd):
  return os.popen(cmd).read()

if (len(sys.argv) <> 2):
  print "usage: %s size_in_bytes" % sys.argv[0]
else:
  maxSize = int(sys.argv[1])

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
      path = "".join(splitdata[1:len(splitdata)-3])
      if (size > maxSize):
        path = path.split("\x00100")
        bigfiles.add("%10d %s %s" % (size, commit, path[0]))

  bigfiles = sorted(bigfiles, reverse=True)

  for f in bigfiles:
      print f
