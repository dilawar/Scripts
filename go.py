#!/usr/bin/env python
import sqlite3 as sql
import os
import sys

configPath = os.environ['HOME']+"/.cdhistory.sqlite"
conn = sql.connect(configPath)

def initDB() :
  global conn
  c = conn.cursor()
  c.execute("""CREATE TABLE IF NOT EXISTS cdh 
    (dir TEXT NOT NULL 
    , date DATE NOT NULL 
    , count INT 
    , PRIMARY KEY(dir, date))""")
  conn.commit()

def changeDir(arg, direction) :
  if arg.isdigit() :
    print("Go level {0} in {1} dirction".format(arg, direction))
  else :
    print("Search for pattern {0} in {1} direction".format(arg, diretction))

if __name__ == "__main__" :
  initDB()
  if len(sys.argv) == 1 :
    print("Display history and ask to select one.")
  elif len(sys.argv) > 1 :
    try :
      if sys.argv[2] == '+' :
        dir = "up"
      elif sys.argv[2] == '-' :
        dir = "down"
      else :
        print("[WARN] Assuming +")
        dir = "up"
    except IndexError :
      print("[WARN] Assuming +")
      dir = "up"
    print("Check for int or string and work accordingly.")
    changeDir(sys.argv[1], dir)
