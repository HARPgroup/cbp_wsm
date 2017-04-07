#!/bin/bash

## Begin functions.inc.sh detection and inclusion
basepath=`dirname $0`
functions_inc="$basepath/functions.inc.sh"

if [ -e "$functions_inc" ] ; then
        . $functions_inc
else
        echo "ERROR: functions.inc.sh doesn't exist or can't be read"
        echo "ERROR: functions.inc.sh should exist in \"$PWD/$basepath\""
        exit 1
fi

## End functions.inc.sh detection and inclusion

fname="somewhere/over/the/rainbow/barA42105.out"
echo "filename: $fname"
echo "landseg: `get_landseg_from_filename $fname`
echo "subshed: `get_subshed_from_filename $fname`
