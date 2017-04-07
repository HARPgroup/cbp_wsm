#!/bin/sh
#
#$Id: get_uptake_nitr.sh 16 2009-04-27 21:06:56Z aknister $
#$Revision: 16 $

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

echo "projectid,scenarioid,model_scen,landseg,luname,thisyear,constit,annual_vol,annual_lo_return,annual_uptake,annual_eof" > $anout

echo "landseg,constit,mean_uptk,mean_eof" > $meanout

# $1 = projectid default:65
# $2 = scenarioid default:1
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
#echo "Searching For $files "

for j in `ls $files`; do
   # must comment this since it goes to a file
   #echo "Parsing $j "
   
   # pull apart the file name to get the relevant info
   # expects the file name convention to be static, in the form
   # of a 3 char lu name, 6 character landseg, which is 1 letter and 
   # a 5 char stcofips

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
   lasttotal=0
   baseyear=$syear
   cumulativen=0
   cumeof=0
   # search the file for each instance of the section header NITR
   # fgrep -n send the format line_number:line_text,
   # awk, by default parses a line by spaces, but -F: asks it to 
   # parse by : (other delimiters can be used similarly)
   for i in `fgrep -n "*** NITR ***" $fname | awk -F: '{print int($1)}'`; do 
      sline=$i
      
      # the total line is located 14 lines from the header line
      # denitrification line
      dnline=`expr $sline + 63`
      # ammonia volatilization line
      nvline=`expr $sline + 87`
      # labile organic line
      loline=`expr $sline + 47`
      # eof line
      eofno=`expr $sline + 43`
      # ammonia uptake line
      nh4no=`expr $sline + 55`
      # nitrate uptake line
      no3no=`expr $sline + 59`
      
      if [ $3 == 'alf' ]; then
      # alf does not have applications so its line numbers are adjusted
         eofno=`expr $sline + 39`
         nh4no=`expr $sline + 51`
         no3no=`expr $sline + 55`
         dnline=`expr $sline + 59`
         nvline=`expr $sline + 63`
         loline=`expr $sline + 43`
         nfixno=`expr $sline + 83`
      fi

      if [ $3 == 'for' ]; then
         # modified 12/4/2006 RWB - 
         eofno=`expr $sline + 42`
         dnline=`expr $sline + 81`
         # no volatilization of nh3 in forest, so set to denit total (must make a note of this)
         nvline=`expr $sline + 81`
         loline=`expr $sline + 47`
      fi
      if [ $3 == 'hvf' ]; then
         eofno=`expr $sline + 35`
      fi
      
      if [ $3 == 'hyo' ]; then
      # hyo does not have applications so its line numbers are adjusted
         eofno=`expr $sline + 39`
         nh4no=`expr $sline + 51`
         no3no=`expr $sline + 55`
         dnline=`expr $sline + 59`
         nvline=`expr $sline + 63`
         loline=`expr $sline + 43`
      fi
      
      if [ $3 == 'pas' ]; then
      # hyo does not have applications so its line numbers are adjusted
         eofno=`expr $sline + 39`
         nh4no=`expr $sline + 51`
         no3no=`expr $sline + 55`
         dnline=`expr $sline + 59`
         nvline=`expr $sline + 83`
         loline=`expr $sline + 43`
      fi

      if [ $3 == 'for' ]; then
         anuptk=0
         no3sub=0
         nh4sub=0
         losub=0
         # get uptake totals for forest
         for x in 0 4; do
            # modified 12/4/2006 RWB - 
            #nh4no=`expr $sline + 61 + $x`
            #no3no=`expr $sline + 69 + $x`
            #loline=`expr $sline + 47 + $x + $x`
            nh4no=`expr $sline + 65 + $x`
            no3no=`expr $sline + 73 + $x`
            loline=`expr $sline + 49 + $x + $x`
            # end modifications
            no3line=`head -n $no3no $fname | tail -n 1`
            nh4line=`head -n $nh4no $fname | tail -n 1`
            no3total=`echo $no3line | awk '{print $5}'`
            nh4total=`echo $nh4line | awk '{print $5}'`
            # get labile organic return
            totlo=`head -n $loline $fname | tail -n 1`
            anloret=`echo $totlo | awk '{print $5}'`
            losub=`echo "$losub $anloret" | awk '{print($1 + $2)}'`
            anuptk=`echo "$no3total $nh4total $anuptk" | awk '{print($1 + $2 + $3)}'`
            no3sub=`echo "$no3sub $no3total" | awk '{print($1 + $2)}'`
            nh4sub=`echo "$nh4sub $nh4total" | awk '{print($1 + $2)}'`
         done
         nh4total=$nh4sub
         no3total=$no3sub
         anloret=$losub
      else
         # get uptake totals for non-forest
         no3line=`head -n $no3no $fname | tail -n 1`
         nh4line=`head -n $nh4no $fname | tail -n 1`
         no3total=`echo $no3line | awk '{print $5}'`
         nh4total=`echo $nh4line | awk '{print $5}'`
         anuptk=`echo "$no3total $nh4total" | awk '{print($1 + $2)}'`
         # get labile organic return
         totlo=`head -n $loline $fname | tail -n 1`
         anloret=`echo $totlo | awk '{print $5}'`
      fi

      # nitrogen fixation
      nfixtotal=0
      if [ $3 == 'alf' ]; then
         nfixline=`head -n $nfixno $fname | tail -n 1`
         nfixtotal=`echo $nfixline | awk '{print $5}'`
      fi
      if [ $3 == 'nal' ]; then
         nfixline=`head -n $nfixno $fname | tail -n 1`
         nfixtotal=`echo $nfixline | awk '{print $5}'`
      fi
      # update to include fixation
      anuptk=`echo "$anuptk $nfixtotal" | awk '{print($1 + $2)}'`
      
      # get edge of fields
      eofline=`head -n $eofno $fname | tail -n 1`
      tneoftotal=`echo $eofline | awk '{print $8 + 0.0}'`
      nh4eoftotal=`echo $eofline | awk '{print $5 + 0.0}'`
      n03eoftotal=`echo $eofline | awk '{print $6 + 0.0}'`
      
      # get volatilization and denitrification
      totdn=`head -n $dnline $fname | tail -n 1`
      andn=`echo $totdn | awk '{print $5 + 0.0}'`
      totnv=`head -n $nvline $fname | tail -n 1`
      annv=`echo $totnv | awk '{print $5 + 0.0}'`
      anvol=`echo "$andn $annv" | awk '{print($1 + $2)}'`

      # print the output to the stdout
      echo $prj,$scn,$4,$landseg,$landuse,$syear,totn,$anvol,$anloret,$anuptk,$tneoftotal >> $anout
      echo $prj,$scn,$4,$landseg,$landuse,$syear,no3n,$andn,-1,$no3total,$n03eoftotal >> $anout
      echo $prj,$scn,$4,$landseg,$landuse,$syear,nh3n,$annv,-1,$nh4total,$nh4eoftotal >> $anout
      if [ $syear > $baseyear ];then
         cumulativen=`echo "$cumulativen  $anuptk" | awk '{print($1 + $2)}'`
         cumeof=`echo "$cumeof  $tneoftotal" | awk '{print($1 + $2)}'`
         numyears=`expr $numyears + 1`
      fi
     # echo "$cumulativen + $anuptk"
      syear=`expr $syear + 1`

      # stash the current year total for use next loop
      lasttotal=$antotal
   done
   avgn=`echo "$cumulativen $numyears" | awk '{print($1 / $2)}'`
   avgeof=`echo "$cumeof $numyears" | awk '{print($1 / $2)}'`
   echo "$landseg,totn,$avgn,$avgeof" >> $meanout
done


