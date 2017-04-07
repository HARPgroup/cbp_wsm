#!/bin/bash

echo "projectid,scenarioid,subshedid,landseg,luname,thisyear,constit,layer,thisvalue"

# $1 = projectid 
# $2 = scenarioid
# $3 = landuse
# $4 = scenario name (such as nut and newnut)
# $5 = land segment ( or 'all' for all)

if [ $5 == 'all' ]; then
   files="../output/hspf/land/out/$3/$4/*.out"
else
   files="../output/hspf/land/out/$3/$4/$3$5.out"
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

   subshed=`echo $j | awk -F/ '{print(substr($5, 5, 5))}'`
   landuse=$3
   landseg=`echo $j | awk -F/ '{print(substr($5, 4, 6))}'`
   fname=$j

   prj=$1
   scn=$2
   # start year is assumed to be static, this is bad,
   # should probably pass this in as a parameter input
   # or simply grab it from within the file output
   syear=1984
   lasttotal=0
   # search the file for each instance of the section header NITR
   # fgrep -n send the format line_number:line_text,
   # awk, by default parses a line by spaces, but -F: asks it to 
   # parse by : (other delimiters can be used similarly)
   for i in `fgrep -n "*** NITR ***" $fname | awk -F: '{print int($1)}'`; do 
      
      sline=$i
      layer=0     

      
      if [ $3 == 'for' ]; then
         # above ground (-1) and litter layer (0) PLant N
         su=`expr $sline + 8`  
         agline=`head -n $su $fname | tail -n 1`
         su=`expr $sline + 9`  # for 'forest' 
         liline=`head -n $su $fname | tail -n 1`
         agplntN=`echo $agline | awk '{print $3}'`
         litterN=`echo $liline | awk '{print $2}'`      
         
         echo $prj,$scn,$subshed,$landseg,$landuse,$syear,PLNTN,-1,$agplntN
         echo $prj,$scn,$subshed,$landseg,$landuse,$syear,PLNTN,0,$litterN
      fi
               
      # iterate through all 4 layers
      for x in 1 2 4 5; do
         if [ $3 == 'for' ]; then
            su=`expr $sline + 10 + $x`   # for 'forest' 
         else
            su=`expr $sline + 7 + $x`   # for any other land uses
         fi
         if [ $3 == 'hvf' ]; then
            su=`expr $sline + 10 + $x`   # for 'harvested forest'
         fi

         layer=`expr $layer + 1`

         Sline=`head -n $su $fname | tail -n 1`

         # ammonia
         nh4SU=`echo $Sline | awk '{print $3}'`
         nh4AD=`echo $Sline | awk '{print $4}'`

         # nitrate
         no3=`echo $Sline | awk '{print $5}'`

         # labile organic N
         LorgSU=`echo $Sline | awk '{print $6}'`
         LorgAD=`echo $Sline | awk '{print $7}'`      
         Lorgn=`echo "$LorgSU $LorgAD" | awk '{print($1 + $2)}'`

         # refractory organic
         RorgSU=`echo $Sline | awk '{print $8}'`
         RorgAD=`echo $Sline | awk '{print $9}'`
         Rorgn=`echo "$RorgSU $RorgAD" | awk '{print($1 + $2)}'`

         # plant N 
         plntN=`echo $Sline | awk '{print $10}'`

         # print the output to the stdout
         echo $prj,$scn,$subshed,$landseg,$landuse,$syear,AMSU,$layer,$nh4SU 
         echo $prj,$scn,$subshed,$landseg,$landuse,$syear,AMAD,$layer,$nh4AD
         echo $prj,$scn,$subshed,$landseg,$landuse,$syear,NO3,$layer,$no3
         # used to print out both adsorbed and soluble, now only adsorbed
         # echo $prj,$scn,$subshed,$landseg,$landuse,$syear,ORGN,$layer,$Lorgn
         # echo $prj,$scn,$subshed,$landseg,$landuse,$syear,RORGN,$layer,$Rorgn
         echo $prj,$scn,$subshed,$landseg,$landuse,$syear,ORGN,$layer,$LorgAD
         echo $prj,$scn,$subshed,$landseg,$landuse,$syear,RORGN,$layer,$RorgAD
         echo $prj,$scn,$subshed,$landseg,$landuse,$syear,PLNTN,$layer,$plntN
      done
 
      syear=`expr $syear + 1`
   done
done
