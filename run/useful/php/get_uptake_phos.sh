#!/bin/sh
#
#$Id: get_uptake_phos.sh 15 2009-04-27 21:06:33Z aknister $
#$Revision: 15 $

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


anout="../../../output/hspf/land/out/$3/$4/$6"
meanout="../../../output/hspf/land/out/$3/$4/$7"

echo "projectid,scenarioid,model_scen,subshedid,landseg,luname,thisyear,constit,annual_vol,annual_lo_return,annual_uptake,annual_eof" > $anout

echo "landseg,constit,mean_uptk" > $meanout

# $1 = projectid default:65
# $2 = scenarioid default:6
# $3 = landuse
# $4 = scenario name (such as nut and newnut)
# $5 = land segment ( or 'all' for all)
# $6 = output file name
# $7 summary output file

if [ $5 == 'allBay' ]; then
   files="../../../output/hspf/land/out/$3/$4/*.out"
else
   files="../../../output/hspf/land/out/$3/$4/$3$5.out"
fi

# must comment this since it goes to a file
echo "Searching For $files "

for j in `ls $files`; do
   # must comment this since it goes to a file
   #echo "Parsing $j "
   
   # pull apart the file name to get the relevant info
   # expects the file name convention to be static, in the form
   # of a 3 char lu name, 6 character landseg, which is 1 letter and 
   # a 5 char stcofips

   subshed=`get_subshed_from_filename $j`
   landuse=$3
   landseg=`get_landseg_from_filename $j`
   fname=$j

   prj=$1
   scn=$2
   # start year is assumed to be static, this is bad,
   # should probably pass this in as a parameter input
   # or simply grab it from within the file output
   syear=1984
   numyears=0
   baseyear=$syear
   lastuptk=0

   # search the file for each instance of the section header NITR
   # fgrep -n send the format line_number:line_text,
   # awk, by default parses a line by spaces, but -F: asks it to 
   # parse by : (other delimiters can be used similarly)
   for i in `fgrep -n "*** PHOS ***" $fname | awk -F: '{print int($1)}'`; do 
      sline=$i
      
      # the total line is located 14 lines from the header line
      # phos uptake line
      po4no=`expr $sline + 13`
      # po4 immobilization line
      imline=`expr $sline + 36`
      # organic P mineralization line
      pmline=`expr $sline + 40`
      # P outflow line
      poutno=`expr $sline + 32`
      
      # get uptake totals for non-forest
      po4line=`head -n $po4no $fname | tail -n 1`
      plantp=`echo $po4line | awk '{print $5}'`
      
      # get edge of field loss for total P
      poutline=`head -n $poutno $fname | tail -n 1`
      pout=`echo $poutline | awk '{print $5}'`

      # get uptake of first year
      if [ $syear = 1984 ];then
         firstuptk=`echo $plantp | awk '{print($1)}'`
      fi
      
      # calculate uptake by subtracting previous value
      anuptk=`echo "$plantp $lastuptk" | awk '{print($1 - $2)}'`

      if [ $syear > $baseyear ];then
         numyears=`expr $numyears + 1`
         lastuptk=`echo "$plantp" | awk '{print($1)}'`
      fi
      
      # print out to the annual file
      echo $prj,$scn,$4,$subshed,$landseg,$landuse,$syear,totp,0,0,$anuptk,$pout >> $anout
      
     # echo "$cumulativen + $anuptk"
      syear=`expr $syear + 1`
   done
     # first year is excluded
     nyear=`expr $numyears - 1`
     avgn=`echo $lastuptk $firstuptk $nyear | awk '{print(($1-$2)/$3)}'`
     echo $landseg,totp,$avgn >> $meanout
done


