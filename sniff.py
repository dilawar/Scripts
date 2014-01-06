#!/usr/bin/env python
import os
import sys
import re
 
class Match:
    def __init__(self):
        self.path = None
        self.filename = None
        self.lines = list()
        self.matchIndex = 0.0
        self.nos = 0
        self.weight = 0.0

    def __str__(self):
        return "{0}: {1}".format(self.path, self.nos) 
    
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

def filter(pat):
    global files
    pat = re.compile(pat, re.IGNORECASE | re.DOTALL)
    for ff in files:
        with open(ff, "r") as f:
            txt = f.read()
            l = pat.findall(txt)
            if len(l) > 0: 
                match = Match()
                match.path = ff
                match.filename = os.path.basename(ff)
                match.nos = len(l)
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
    if len(sys.argv) > 1:
        filepattern = sys.argv[3]
    else:
        filepattern = '.*'
    main(dir, pattern, filepattern)


