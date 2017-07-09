#!/bin/sh

##
##usage: validateSBML.sh [-h] [-d opt1[,opt2,...]] [-o output] filename.xml
##       validateSBML.sh [-h] [-d opt1[,opt2,...]] [-o output] http://.../filename.xml
##
## Validates the SBML document given by filename.xml using the
## SBML.org Online Validator.
##

#
# @file    validateSBML.sh
# @brief   Validates an SBML document using the SBML.org Online Validator
# @author  Ben Bornstein <sbml-team@caltech.edu>
# @author  Akiya Jouraku <sbml-team@caltech.edu>
#
# $Id$
# $Source$
#
# Copyright 2006 California Institute of Technology and
# Japan Science and Technology Corporation.
#
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation; either version 2.1 of the License, or
# any later version.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
# documentation provided hereunder is on an "as is" basis, and the
# California Institute of Technology and Japan Science and Technology
# Corporation have no obligations to provide maintenance, support,
# updates, enhancements or modifications.  In no event shall the
# California Institute of Technology or the Japan Science and Technology
# Corporation be liable to any party for direct, indirect, special,
# incidental or consequential damages, including lost profits, arising
# out of the use of this software and its documentation, even if the
# California Institute of Technology and/or Japan Science and Technology
# Corporation have been advised of the possibility of such damage.  See
# the GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this library; if not, write to the Free Software Foundation,
# Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
#

# Print the usage statement at the top of this program and available options.
usage() {
  synopsis=`grep "^##" $0 | sed 's/##//'`
  cat << EOF
$synopsis

options:

  -o output          
    Specify an output format.

      text  : plain text (Default)
      xml   : XML 
      xhtml : XHTML 
      json  : JavaScript Object Notation

  -d opt1[,opt2,...] 
    Disable the given consistency check options. 
    The options are given as comma-separated characters.
    Each character is one of the followings:

      u : disable the units consistency check
      g : disable the overall SBML consistency check
      i : disable the identifier consistency check
      m : disable the MathML consistency check
      s : disable the SBO consistency check
      o : disable the overdetermined model check
      p : disable the modeling practice check

  -h  Print this usage.
EOF

}

validator_url='http://sbml.org/validator/'
output='text'

# Parse the command-line arguments.
while getopts d:o:h opt
do
  case $opt in
    d)
      offcheck="-F offcheck=$OPTARG"
      ;;
    o)
      output=$OPTARG
      ;;
    *)
      usage
      exit
      ;;
  esac
done
shift `expr $OPTIND - 1`

# Exit if no SBML file given.
if [ $# -lt 1 ]; then
  usage
  exit
fi

filename=$1

if echo $filename | grep -q '^http://'; then
  src="url=${filename}"  
else
  src="file=@${filename}"  
fi

curl -F output=${output} -F ${src} ${offcheck} ${validator_url}
