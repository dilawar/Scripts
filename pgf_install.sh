#!/bin/bash
echo "Getting current version of tikz"
cvs -d:pserver:anonymous@pgf.cvs.sourceforge.net:/cvsroot/pgf login
cvs -z3 -d:pserver:anonymous@pgf.cvs.sourceforge.net:/cvsroot/pgf co -P pgf
echo "Compiling using luatex"
cd pgf/doc/generic/pgf/version-for-luatex/en
LUAINPUTS=".:../../../../..//:" TEXINPUTS=".:../../../../..//:" make
LUAINPUTS=".:../../../../..//:" TEXINPUTS=".:../../../../..//:" make dist
LUAINPUTS=".:../../../../..//:" TEXINPUTS=".:../../../../..//:" make dist
cd ../../../../../..
# create TDS archive
make -f pgf/scripts/pgf/Makefile.pgf_release
cp pgf/doc/generic/pgf/ChangeLog pgf_CVS.ChangeLog
