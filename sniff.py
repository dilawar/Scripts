#!/usr/bin/env python
import os
import sys
import re
 
# Definitions of colors in bash
RESTORE='\033[0m'
RED='\033[00;31m'


class Match:
    def __init__(self):
        self.path = None
        self.filename = None
        self.lines = list()
        self.matchIndex = 0.0
        self.nos = 0

    def __str__(self):
        for i in range(len(self.lines)):
            if i == 0: pass
            else:
                self.lines[i] = self.lines[i-1] + self.lines[i] - 1
        # Last entry is not the position of match. remove it
        self.lines.pop()
        txt = RED+self.path+RESTORE+':'
        for l in self.lines:
            txt += '{} '.format(l)
        return txt
    
    def __repr__(self):
        return self.__str__()


files = set()
result = list()

def insertIntoResult(match):
    global result
    i = 0
    if len(result) == 0: 
        result.append(match)
        return
    for r in result:
        if match.matchIndex > r.matchIndex:
            result.insert(i, match)
            return
        else: i += 1
    result.append(match)

def filter(pat):
    global files
    # Do some intelligent stuff with pattern
    #1. if space is given in pattern replace it with \s+
    pat = re.sub('\s+', '\s+', pat)
    pat = re.compile(pat, re.DOTALL)
    for ff in files:
        with open(ff, "r") as f:
            txt = f.read()
            l = pat.split(txt)
            if len(l) > 1: 
                lines = []
                for i in l:
                    lines.append(len(i.split('\n')))
                match = Match()
                match.path = ff
                match.filename = os.path.basename(ff)
                match.lines = lines
                match.matchIndex = match.nos
                insertIntoResult(match)


def main(dir, pat, filepat):
    global files
    global result
    for d, ds, fs in os.walk(dir):
        for f in fs:
            filename = os.path.join(d, f)
            if re.match(filepat, filename):
                files.add(filename)
    filter(pat)
    for r in result:
        print r

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("USAGE: {0} dir pattern [file_pattern]".format(sys.argv[0]))
        sys.exit(0)

    dir = sys.argv[1]
    if not os.path.isdir(dir):
        print("First arguement should be a directory")
        sys.exit(0)

    pattern = sys.argv[2]
    if len(sys.argv) > 3:
        filepattern = sys.argv[3]
    else:
        filepattern = '.*'
    main(dir, pattern, filepattern)


