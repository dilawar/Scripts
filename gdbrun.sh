#!/bin/bash

#http://paraview.org/Wiki/CMake_Platform_Dependent_Issues

#The following is an extremely useful script that will run any command line in a gdb debugger. Put this text in an executable file called "gdbrun": 
# Then one may be able to debug as-such:
#		gdbrun /path/to/myexe --some-arg --some-other-arg

#Notes about this script:

#   * It supports spaces in argument names (note the for loop)
#    * Takes extra argument --break-main, which causes the program to stop once all the libraries are loaded
#    * It always run debugger, even when program exits normally
#    * Cannot be used with MPI or any other system that runs your program from a shell script 

extra_text=""
if [ "$1" == "--break-main" ]; then
  extra_text="break main"
  shift
fi

EXEC="$1"

shift

run_text="run"
for a in "$@"; do
  run_text="${run_text} \"$a\""
done

TMPFILE=/tmp/gdbrun.$$.$#.tmp
cat > ${TMPFILE} <<EOF
${extra_text}
${run_text}
EOF

gdb -x ${TMPFILE} "${EXEC}"
rm -f "${TMPFILE}"

#
