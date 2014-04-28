#!/usr/bin/env python
''' 
This script compares two directories.

Launch vimdiff if there are moderate changes in two files with same name.
'''
from __future__ import division
import difflib
import os
import sys
import argparse
import subprocess

import string 

def istext(filename):
    ''' This function is from 
    http://stackoverflow.com/questions/1446549/how-to-identify-binary-and-text-files-using-python
    '''
    s=open(filename).read(512)
    text_characters = "".join(map(chr, range(32, 127)) + list("\n\r\t\b"))
    _null_trans = string.maketrans("", "")
    if not s:
        # Empty files are considered text
        return True
    if "\0" in s:
        # Files with null bytes are likely binary
        return False
    # Get the non-text characters (maps a character to itself then
    # use the 'remove' option to get rid of the text characters.)
    t = s.translate(_null_trans, text_characters)
    # If more than 30% non-text characters, then
    # this is considered a binary file
    if len(t)/len(s) > 0.30:
        return False
    return True

def insertIntoDict(fileName, dirName, dictName):
    """Insert a filename inside dirName into a dict
    Given a file with fileName, residing in dirName, insert it into a
    dictionary.

    dict[fileName] = full path of file filename
    """
    dictName[fileName] = os.path.join(dirName, fileName)

def filterExtensions(dict, extensions):
    '''Remove files which are not in extensions.
    '''
    global args

    textFileDict = {}
    if args.enable_binary:
        textFileDict = dict
    else:
        # Collect only binary files.
        for f in dict:
            if not istext(dict[f]):
                textFileDict[f] = dict[f]

    if extensions is None:
        print("No extensions found")
        return dict
    newDict = {}
    for f in textFileDict:
        ext = f.split('.')[-1]
        if ext in extensions:
            newDict[f] = textFileDict[f]
        else:
            pass
    return newDict

def main():
    global args
    assert args.directoryA
    assert args.directoryB

    filesA = dict()
    for root, subdir, file in os.walk(args.directoryA):
        [ insertIntoDict(f, root, filesA) for f in file ]

    filesB = dict()
    for root, subdir, file in os.walk(args.directoryB):
        [ insertIntoDict(f, root, filesB) for f in file ]

    filesA, filesB = [ filterExtensions(d, args.extension) 
            for d in [filesA, filesB] 
            ]
    if len(filesA) < 1 or len(filesB) < 1:
        print("One or more directories have no files inside them")
        return

    diff(filesA, filesB)

def diff(dictA, dictB):
    ''' Diff two files given in two directories '''
    [diffFile(f, dictA[f], dictB) for f in dictA ]

def diffFile(fileA, fileAPath, dictB):
    global args
    fileBPath = dictB.get(fileA, None)
    if fileBPath is None:
        pass
    else:
        checkFilesAndLaunchDiff(fileAPath, fileBPath)

def checkFilesAndLaunchDiff(fileAPath, fileBPath):
    import difflib
    global args
    with open(fileAPath, "r") as f1:
        textA = f1.read()
    with open(fileBPath, "r") as f2:
        textB = f2.read()

    seq = difflib.SequenceMatcher(None, textA, textB).ratio()
    if seq == 1.0:
        msg = "{} and {} are same. \n".format(fileAPath, fileBPath)
        msg += "|- q to exit, other to continue: "    
        printAndExit(msg)
    elif seq <= 0.5:
        msg = "{} and {} are too different\n.".format(fileAPath, fileBPath)
        msg += "|- q to exit, other to continue: "    
        printAndExit(msg)
    else:
        launchDiffTool(fileAPath, fileBPath)

def printAndExit(msg):
    if raw_input(msg) is not 'q':
        return
    else:
        sys.exit(0)
        
def launchDiffTool(fileA, fileB, diffTool='vimdiff'):
    ''' Given two file paths, launch them in a difftool '''
    cmd = [diffTool, fileA, fileB]
    subprocess.call(cmd)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Diff two directories")
    parser.add_argument("-v", "--verbose"
            , help="Verbose output"
            )
    parser.add_argument("-A", "--directoryA"
            , required = True
            , help = "Directory A"
            )
    parser.add_argument("-B", "--directoryB"
            , required = True
            , help = "Directory B"
            )
    parser.add_argument("-e", "--extension"
            , nargs = "*"
            , help = "Space separated list of extensions. Ignore all other files."
            )
    parser.add_argument("-b", "--enable_binary"
            , default = False
            , help = "Enable comparisions of binary files. Untested feature"
            )
    args = parser.parse_args()
    main()

