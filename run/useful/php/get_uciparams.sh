#!/bin/bash

# inputs to this script
# $1 = landuse
# $2 = scenario name (such as nut and newnut)
# $3 = land segment ( or 'all' for all)
# $4 = section
# $5 = block
# $6 = parameter
# $7 = scenarioid

if [ $1 == "--help" ];then
   echo "Usage: ./get_uciparams.sh [luname] [scenario] [lseg] [section] [block] [param] [scenarioid]"
else

# inputs needed for report_uciparams.php
#      $projectid
#      $scenarioid
#      $fullpathtofile
#      $thissection (such as PERLND or IMPLND)
#      $thisblock (parameter block such as NIT-FSTPM )
#      $thisparam (parameter in block, such as KIMAM )
#      $landseg (such as A42105)
#      $subshedid (such as 42105 )
#      $luname (such as lwm)

# get the current directory tree from the csh environment scripts
tree=`./get_tree.csh`

# set up default
prj=65
scn=$7

if [ $3 == 'all' ]; then
   files="$tree/uci/land/$1/$2/*.uci"
else
   files="$tree/uci/land/$1/$2/$1$3.uci"
fi

# must comment this since it goes to a file
#echo "Searching For $files "

for j in `ls $files`; do
   subshed=`echo $j | awk -F/ '{print(substr($9, 5, 5))}'`
   landuse=$1
   landseg=`echo $j | awk -F/ '{print(substr($9, 4, 6))}'`
   fname=$j
   echo "Processing $landseg"
   
   php -q report_uciparam.php $prj $scn $j $4 $5 $6 $landseg $subshed $1 $2
done

fi
