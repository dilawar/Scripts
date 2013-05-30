#!/bin/bash -x

c () 
{
  dbname=$HOME/.cdsqlite
  if [ ! -f $dbname ] ; then 
    tableCommand="CREATE TABLE IF NOT EXISTS \
      cdh (dirname TEXT PRIMARY KEY, count INTEGER, accessed date);" 
    cat /dev/null > $dbname
    echo $tableCommand > /tmp/structure
    sqlite3 $dbname < /tmp/structure 
  fi
  dir=~
  # If no argument is given the present the most used directory paths in during
  # last week.
  if [[ $# == 0 ]] ; then 
    dir=~
  else 
    dir=$1
  fi
  cd $dir
  if [ $? -eq 0 ]; then 
    dir=$(pwd)
    (
      sqlite3 $dbname "INSERT OR IGNORE INTO cdh (dirname, count, accessed) 
        VALUES ('$dir', '1', datetime('now')); 
        UPDATE cdh SET count=count + 1 
        where dirname LIKE '$dir';" &
    )
  else 
    echo "I can't change to : $dir"
  fi
}

