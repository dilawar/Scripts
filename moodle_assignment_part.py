#!/usr/bin/env python

import sys
import tarfile, zipfile
import os, shutil, subprocess
import glob
from collections import defaultdict



inputFile = sys.argv[1]
curDir = os.getcwd()

assignments = defaultdict(list)

def extract_asssignments(dir):
        path = dir
        os.chdir(path)
        listing = glob.glob(path+'/*gz')
        for file in listing:
            print " |- Extracting archive ...{0}".format(file)
            subprocess.call(["tar", "xzvf", file], stdout=subprocess.PIPE)

        listing = glob.glob(path+'/*bz')
        for file in listing:
            print " |- Extracting archive ...{0}".format(file)
            subprocess.call(["tar", "xjvf", file], stdout=subprocess.PIPE)

        listing = glob.glob(path+'/*zip')
        for file in listing:
            print " |- Extracting archive ...{0}".format(file)
            subprocess.call(["unzip", "-o", file], stdout=subprocess.PIPE)

        listing = glob.glob(path+'/*rar')
        for file in listing:
            print " |- Extracting archive ...{0}".format(file)
            subprocess.call(["unrar", "x", "-o+", file], stdout=subprocess.PIPE)

        listing = glob.glob(path+'/*tar')
        for file in listing:
            print " |- Extracting archive ...{0}".format(file)
            subprocess.call(["tar", "xvf", file], stdout=subprocess.PIPE)



with zipfile.ZipFile(inputFile, "r") as myzip :
    listobj = myzip.infolist()
    for obj in listobj :
        zippedFile = obj.filename
        filename = zippedFile.split("_")
        studentName = filename[0].strip()
        file = filename[1]
        assignments[studentName].append(file)

        path = curDir+"/"+studentName
        if os.path.exists(path) :
            shutil.rmtree(path)
            os.mkdir(path)
        else:
            os.mkdir(path)
        myzip.extract(zippedFile, path)
        pathToExtract= curDir+"/"+studentName+"/"+zippedFile
        os.chdir(path)
        extract_asssignments(path)
        os.chdir(curDir)
