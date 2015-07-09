#!/usr/bin/env python

import libsbml
import sys

def format_error(msg):
    words = msg.split()
    line = []
    lines = []
    for w in words:
        line.append(w)
        lineText = ' '.join(line)
        if len(lineText) > 80:
            line = []
            lines.append(lineText)
    return "\n\t".join(lines)

def read_file(filename):
    reader = libsbml.SBMLReader()
    document = reader.readSBMLFromFile(filename)
    model = document.getModel()

    numError = document.getNumErrors()
    print("|- Total parse errors in doc: %s" % numError)
    for i in range(numError):
        print('+ {}: {}'.format(i, document.getError(i)))

    numError = document.checkConsistency()
    print("|- Total consistency error in model: %s" % numError)
    for i in range(numError):
        print('+ {}: {}'.format(i,
            format_error(document.getError(i).getMessage())))

    model = document.getModel()
    print("|- Total species in model: %s" % model.getNumSpecies())

def main():
    filename = sys.argv[1]
    read_file(filename)

if __name__ == '__main__':
    main()

