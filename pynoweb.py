#!/usr/bin/env python 

"""
This is a noweb front-end. It introduces some more language-constructs.

- \include{file.nw} includes another noweb file.
- %outfile file.py writes fragment of code to file.py.

"""

import os 
import sys 
import subprocess
import argparse
import re
import collections
import shutil

import logging
logging.basicConfig(level=logging.DEBUG)

textQueue = collections.deque()
chunks = dict()
nowebTempDir = "build"
nowebSrcDir = 'src'
nowebDocDir = 'docs'


def doesCommandExist(command):
  res = subprocess.call(["which", command], shell=False, stderr=None
      , stdout=open(os.devnull, 'wb'))
  if res == 0 :
    return True
  else :
    return False

def allIncludes(nowebText) :
  files = []
  inregex = re.compile(r'^%\\include{\s*(?P<filename>[\w\.]+)\s*}\s*$')
  lineno = 0
  for line in nowebText :
    lineno += 1
    m = inregex.match(line) 
    if m :
      filepath = os.getcwd() + '/' + m.group('filename')
      if os.path.isfile(filepath) :
        print("Appending {} to files".format(filepath))
        files.append((filepath, lineno))
      else :
        print("Warn : Can't open included file {0}. Ignoring it. It will cause \
            error with noweb".format(filepath))
    else : pass

    # check if a chunk is to be written to file 
    chunkRegex = re.compile(r'^%outfile\s*(?P<tofile>[\w\.\/]+)\s*')
    mm = chunkRegex.match(line)
    if mm :
      # look-ahead in next line to get the name of the chunk
      mmm = re.match(r'^\<\<(?P<name>[\w\.]+)\>\>=\s*$', nowebText[lineno])
      if mmm :
          logging.debug("++ Found a chunk %s" % mmm)
          chunkName = mmm.group('name')
          chunks[chunkName] = mm.group('tofile')
  return files
    
def mergeFiles(fileH) :
    global textQueue
    logging.info("Processing for merging: {0}".format(fileH.name))
    fileTxt = fileH.readlines()
    markA = 0
    markB = 0
    files = allIncludes(fileTxt)
    if len(files) == 0 :
        textQueue.append(fileTxt)
        fileH.close()
        return
    else :
        for (filename, lineno) in files:
            markB = lineno 
            text = fileTxt[markA:markB-1]
            markA = markB + 1
            textQueue.append(text)
            with open(filename, "r") as f:
                mergeFiles(f)
        # append whatever is left in file
        textQueue.append(fileTxt[markA:])
        fileH.close()

def finalText() :
    text = []
    for t in textQueue:
        text = text + t
    return "".join(text)


def executeCommand(command, outFile = None) :
    if outFile:
        with open(outFile, "w") as outF :
            print("= Executing : {0} > {1}".format(
                " ".join(command), outFile)
                )
            subprocess.call(command, stdout=outF)
    else:
        print("= Executing : {0}".format(
            " ".join(command), outFile)
            )
        subprocess.call(command, stdout=sys.stdout)



def executeNoweb(args, nowebTempDir):
    mainFilepath = os.path.join(nowebTempDir, args['file'].name)
    if args['tangle'] is not None:
        for chunk in chunks:
            nowebCommand = ["notangle"]
            # Now generate files for chunk.
            logging.info("+ Writing {0} chunk to : {1}".format(
                chunk
                , chunks[chunk])
                )
            nowebCommand.append("-R{0}".format(chunk))
            nowebCommand.append(mainFilepath)
            executeCommand(nowebCommand, chunks[chunk])

    if args['weave'] is not None:  # must be weaving 
        outFile = args.get('output', None)
        nowebCommand = ["noweave", "-latex" , "-n", "-delay" ] + [mainFilepath.strip()] 
        executeCommand(nowebCommand, outFile)
    else:
        logging.debug("Using default arguements")
        args['tangle'] = ['tangle']
        args['weave'] = ['weave']
        args['output'] = os.path.join(
                nowebTempDir
                , args['file'].name+'.tex'
                )
        return executeNoweb(args, nowebTempDir)


if __name__ == "__main__" :
    
    if not doesCommandExist("noweb") :
        msg = "Command noweb can not be launched on this system. Quitting..."
        logging.error(msg)
        sys.exit()

    # read the top-most file 
    parser = argparse.ArgumentParser(description='Front end of noweb')
    parser.add_argument('-f'
            , '--file'
            , type=argparse.FileType('r', 0)
            , required = True 
            , help = 'Just pass the top-most noweb file after --file '
            )
    parser.add_argument('-t'
            , '--tangle'
            , type = str
            , nargs = '*'
            , help = "Tangle. Pass optional argument to notangle."
            )
    parser.add_argument('-w'
            , '--weave'
            , type = str
            , nargs = '*'
            , help = "Weave. Pass optional arguemnt to noweave."
            )
    parser.add_argument('-o'
            , '--output'
            , type = str
            , help = 'Filename of latex'
            )

    args = vars(parser.parse_args())
    
    # merge all noweb files.
    mergeFiles(args['file'])
    if len(chunks) == 0:
        logging.warning("++ No chunks found in files")

    # create a temp folder for pynoweb so that this application can work without
    # hurting others.
    if not os.path.exists(nowebTempDir):
        os.makedirs(nowebTempDir)
    else:
        shutil.rmtree(nowebTempDir+"/*", ignore_errors=True)

    with open(os.path.join(nowebTempDir, args['file'].name), "w") as finalF:
        finalF.write(finalText())
    executeNoweb(args, nowebTempDir)
  
