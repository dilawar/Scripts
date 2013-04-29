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

def insertIntoDB(dir) :
  global conn 
  c = conn.cursor()
  c.execute("""INSERT OR IGNORE INTO cdh 
    (dir, date) VALUES ('{0}', 'now', '1')""".format(dir))
  c.execute("""UPDATE cdh SET count = count + 1 WHERE dir LIKE\
      '{0}'""".format(dir))
  conn.commit()


def changeDir(arg, direction) :
  if arg.isdigit() :
    print("[I] Go level {0} in {1} dirction".format(arg, direction))
  else :
    print("[I] Search for pattern {0} in {1} direction".format(arg, direction))
    import re 
    currentDir = os.getcwd()
    newDir = os.path.join(currentDir, arg)
    if os.path.exists(newDir) :
      print("[I] Path exists. Changing to {0}".format(newDir))
      import subprocess 
      subprocess.call("cd {0}".format(newDir), shell = True)
    else :
      print("[I] This path must be searched.")

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
    changeDir(sys.argv[1], dir)
