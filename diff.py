#!/usr/bin/env python 
import sys, os
import time

def checkTwoFiles(string) :
  # This is already verified.
  if string.strip()[0] == "#" :
    return 
  files = string.split(",")
  filesToCompare = list()
  for file in files :
    file = file.strip()
    file = file.replace('"', '')
    if os.path.isfile(file) :
      filesToCompare.append(file.replace(' ','\ '))
  command = "vimdiff "
  for file in filesToCompare :
    command += (file+" ")
  os.system(command)

if __name__ == "__main__" :
  if len(sys.argv) < 2 :
    print("USAGE : ./diff.py [-f filename] OR [filenames separated by comma]")
    sys.exit(0)

  fileMode = False 
  filePath = ""


  if sys.argv[1] == "-f" :
    fileMode = True 
    filePath = sys.argv[2]
    print("Processing file : {0}".format(filePath))

  if fileMode :
    verified = ""
    # If verified files list exists then load it.
    verifyFilePath = filePath+"_verified"
    if os.path.isfile(verifyFilePath) :
      verified = open(verifyFilePath, "r").read()

    
    # And append new entries
    print filePath 
    txt = open(filePath, "r").read()
    
    lines = txt.split("\n")
    newText = ""
    keepGoing = True 
    while len(lines) > 0 and keepGoing :
      line = lines.pop()
      if verified.find(line) != -1 :
        print("These files are already accepted. continuing ... ")
        continue 
      string = line[:]
      checkTwoFiles(string)
      res = raw_input("Press any character to reject [x to exit]:")
      if len(res.strip()) == 0 :
          newline = time.strftime("%Y-%m-%D:%t")+" == "+line
          verified += newline+"\n"
          # remove this line from origial list 
      elif res.strip() == "x" :
        keepGoing = False 
      else : pass 
      # write back the modified text.
      newText += "#"+line+"\n"
    
    os.remove(filePath)
    with open(filePath, "w") as outF :
      outF.write(newText)
      # write the unprocessed lines.
      for line in lines :
        outF.write(line+"\n")

    with open(verifyFilePath, "a") as vf :
      vf.write(verified)
  else :
    checkTwoFiles("".join(sys.argv[1:]))
    
