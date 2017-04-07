#!/bin/sh
#
# File to store common functions
#

#$Revision$
#$Id: functions.inc.sh 16 2009-04-27 21:06:56Z aknister $


get_landseg_from_filename() {
	if [ -n "$1" ] ; then
		basename $1  | sed -ne 's/\([a-z]\{3\}\)\([a-zA-Z]\)\([0-9]\+\).out/\2\3/pg'
	else 
		echo "Error: Null string passed into get_landseg_from_filename. ABORTING."
		exit 1
	fi
}

get_subshed_from_filename() {
	if [ -n "$1" ] ; then
		basename $1  | sed -ne 's/\([a-z]\{3\}\)\([a-zA-Z]\)\([0-9]\+\).out/\3/pg'
	else 
		echo "Error: Null string passed into get_subshed_from_filename. ABORTING."
		exit 1
	fi

}
