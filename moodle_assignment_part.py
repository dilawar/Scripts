#!/usr/bin/env python3

import sys
import tarfile, zipfile
import os, shutil, subprocess
import glob
from collections import defaultdict
import errno


assignments = defaultdict(list)

def extract_asssignments(dirs):
  for dir in dirs :
    path = dir
    os.chdir(path)
    listing = glob.glob(path+'/*gz')
    for file in listing:
      print(" |- Extracting archive ...{0}".format(file))
      subprocess.call(["tar", "xzvf", file], stdout=subprocess.PIPE)

    listing = glob.glob(path+'/*bz')
    for file in listing:
      print(" |- Extracting archive ...{0}".format(file))
      subprocess.call(["tar", "xjvf", file], stdout=subprocess.PIPE)

    listing = glob.glob(path+'/*zip')
    for file in listing:
      print(" |- Extracting archive ...{0}".format(file))
      subprocess.call(["unzip", "-o", file], stdout=subprocess.PIPE)

    listing = glob.glob(path+'/*rar')
    for file in listing:
      print(" |- Extracting archive ...{0}".format(file))
      subprocess.call(["unrar", "x", "-o+", file], stdout=subprocess.PIPE)

    listing = glob.glob(path+'/*tar')
    for file in listing:
      print(" |- Extracting archive ...{0}".format(file))
      subprocess.call(["tar", "xvf", file], stdout=subprocess.PIPE)


if __name__ == "__main__" :

  inputFile = sys.argv[1]
  curDir = os.getcwd()

  with zipfile.ZipFile(inputFile, "r") as myzip :
    listobj = myzip.infolist()
    filesToExtract = list()
    print("|- Extracting from zip file.")
    for obj in listobj :
      zippedFile = obj.filename
      filename = zippedFile.split("_")
      studentName = filename[0].strip()
      file = filename[1]
      assignments[studentName].append(file)

      path = curDir+"/"+studentName
      
      try :
        os.makedirs(path)
      except OSError as exception :
        if exception.errno != errno.EEXIST :
          raise 

      myzip.extract(zippedFile, path)
      pathToExtract= curDir+"/"+studentName+"/"+zippedFile
      filesToExtract.append(path)
    extract_asssignments(filesToExtract)

    
