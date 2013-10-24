#!/usr/bin/env python 

"""
This is a noweb front-end and it introduces some more language-constructs.

1. Ability to include files in top-most noweb file.

"""

import os 
import sys 
import subprocess
import argparse
import re
import collections

textQueue = collections.deque()


def doesCommandExist(command):
  res = subprocess.call(["which", command], shell=False, stderr=None
      , stdout=open(os.devnull, 'wb'))
  if res == 0 :
    return True
  else :
    return False

def allIncludes(nowebText) :
  files = []
  inregex = re.compile(r'^\\include{\s*(?P<filename>[\w\.]+)\s*}\s*$')
  lineno = 0
  for line in nowebText :
    lineno += 1
    m = inregex.match(line) 
    if m :
      filepath = os.getcwd() + '/' + m.group('filename')
      if os.path.isfile(filepath) :
        files.append((filepath, lineno))
      else :
        print("Warn : Can't open included file {0}".format(filepath))
    else : pass
  print("Found {0} included files".format(len(files)))
  return files
    
def mergeFiles(fileH) :
  global textQueue
  print("Processing : {0}".format(fileH.name))
  fileTxt = fileH.readlines()
  markA = 0
  markB = 0
  files = allIncludes(fileTxt)
  if len(files) == 0 :
    textQueue.append(fileTxt)
    fileH.close()
    return
  else :
    for (filename, lineno) in files :
      markB = lineno 
      text = fileTxt[markA:markB-1]
      markA = markB + 1
      textQueue.append(text)
      with open(filename, "r") as f :
        mergeFiles(f)
    # append whatever is left in file
    textQueue.append(fileTxt[markA:])
    fileH.close()

def finalText() :
  text = []
  for t in textQueue :
    text = text + t
  return "".join(text)

if __name__ == "__main__" :
  
  if not doesCommandExist("noweb") :
    print("Command noweb can not be launched on this system. Quitting...")
    sys.exit()

  # read the top-most file 
  parser = argparse.ArgumentParser(description='Front end of noweb')
  parser.add_argument('--top', type=argparse.FileType('r', 0)
                     , help='Just pass the top-most noweb file after --top ')
  parser.add_argument('--noweb', type=str
      , help = "noweb arguments except with or without file name")

  args = parser.parse_args()
  
  # read the file 
  mergeFiles(args.top)
  with open(args.top.name+".nw", "w") as finalF :
    finalF.write(finalText())
  
