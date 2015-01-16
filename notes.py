#!/usr/bin/env python
# A python front-end for managing notes.

import sqlite3 as sql
import os
import sys

class Notes():

    def __init__(self):
        self.editor = 'vim'
        self.noteDir = os.path.join(os.environ['HOME'], 'notes')
        if not os.path.isdir(self.noteDir):
            os.makedirs(self.noteDir)
        self.noteDB = os.path.join(self.noteDir, 'notes.sqlite')
        self.connect()

    def connect(self):
        self.db = sql.connect(self.noteDB)
        self.c = self.db.cursor()



def main():
    n = Notes()

if __name__ == '__main__':
    main()
