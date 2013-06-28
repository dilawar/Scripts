#!/bin/bash 
# GNU-GPL
# (c) Dilawar Singh, 2013
# dilawar@ee.iitb.ac.in

c () 
{
  alias cd='cd'
  dbname=$HOME/.cdsqlite
  if [ ! -f $dbname ] ; then 
    tableCommand="CREATE TABLE IF NOT EXISTS \
      cdh (dirname TEXT PRIMARY KEY, count INTEGER, accessed date);" 
    cat /dev/null > $dbname
    echo $tableCommand > /tmp/structure
    sqlite3 $dbname < /tmp/structure 
  fi
  # If no argument is given, fetch most used directory paths in last 3 days.
  if [[ $# == 0 ]] ; then 
    IN=`sqlite3 $dbname "SELECT dirname FROM cdh WHERE 
                  accessed > datetime('now', '-3 days') ORDER BY count DESC"`

    declare -a ch
    read -ra choices <<< $IN 
    count=0
    for d in "${choices[@]}" 
    do 
      ch[count]=$d 
      echo "$count :" $d 
      let count++
    done 
    echo "Your choice [default 0] : "
    read choice 
    if [[ $choice =~ [0-9]+ ]]; then 
      if [[ $choice -ge $count ]]; then 
        echo "Invalid numeric choice. Existing .."
        return
      fi
    else 
      echo "No numeric choice. Using default."
      choice=0
    fi

    ## Good, we have a choice. Now find the directory and cd to it.
    dir=${ch[$choice]}
    c $dir
  else 
    dir=$1 
    cd $dir
    if [[ $? == 0 ]]; then 
    {
      dir=$(pwd)
      (
        sqlite3 $dbname "INSERT OR IGNORE INTO cdh (dirname, count, accessed) 
          VALUES ('$dir', '0', datetime('now')); 
          UPDATE cdh SET count=count + 1, accessed=datetime('now') 
          where dirname LIKE '$dir';" &
      )
    }
    else 
      echo "Searching database for matches ... "
      dir="*$dir*"
      IN=`sqlite3 $dbname "SELECT dirname FROM cdh WHERE dirname GLOB '$dir';"`
      declare -a cch
      read -ra ccc <<< $IN 
      count=0
      for d in "${ccc[@]}" 
      do 
        cch[count]=$d 
        echo "$count :" $d 
        let count++
      done 
      if [[ $count -eq 1 ]]; then 
        dir=${cch[0]}
        c $dir
      else 
        echo "Your choice [default 0] : "
        read choice 
        if [[ $choice =~ [0-9]+ ]]; then 
          if [[ $choice -ge $count ]]; then 
            echo "Invalid numeric choice."
            return
          fi
        else 
          echo "Invalid choice. Using default 0."
          choice=0
        fi
        ## Good, we have a choice. Now find the directory and cd to it.
        dir=${cch[$choice]}
        c $dir
      fi
    fi
  fi
}

