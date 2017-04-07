#!/bin/bash

stout="./land/$1/$2/$4"

echo "landseg,landuse,year,SFmin,UPmin,LOmin,GWmin,TOTmin" > $stout

# $1 = landuse
# $2 = scenario name (such as nut and newnut)
# $3 = land segment ( or 'all' for all)
# $4 = stoarage file

if [ $3 == 'all' ]; then
   files="../output/hspf/land/out/$1/$2/*.out"
else
   files="../output/hspf/land/out/$1/$2/$1$3.out"
fi

# must comment this since it goes to a file
#echo "Searching For $files "

for j in `ls $files`; do
   # must comment this since it goes to a file
  echo "Parsing $j "
   
   # pull apart the file name to get the relevant info
   # expects the file name convention to be static, in the form
   # of a 3 char lu name, 6 character landseg, which is 1 letter and 
   # a 5 char stcofips
   subshed=`echo $j | awk -F/ '{print(substr($5, 5, 5))}'`
   landuse=$1
   sceno=$2
   landseg=`echo $j | awk -F/ '{print(substr($5, 4, 6))}'`
   fname=$j

   # start year is assumed to be static, this is bad,
   # should probably pass this in as a parameter input
   # or simply grab it from within the file output
   syear=1984

   # search the file for each instance of the section header NITR
   # fgrep -n send the format line_number:line_text,
   # awk, by default parses a line by spaces, but -F: asks it to 
   # parse by : (other delimiters can be used similarly)
   for i in `fgrep -n "*** NITR ***" $fname | awk -F: '{print int($1)}'`; do 
      
      sline=$i
      ml=`expr $sline + 75`         # for any other land uses
             
      if [ $1 == 'for' ]; then
        ml=`expr $sline + 93`        # for 'forest' 
      fi

      if [ $1 == 'pas' ]; then
        ml=`expr $sline + 71`        # for 'pasture'
      fi

      if [ $1 == 'hyo' ]; then
        ml=`expr $sline + 71`        # for 'hay without manure'
      fi

      if [ $1 == 'puh' ]; then
        ml=`expr $sline + 71`        # for 'urban'
      fi

      if [ $1 == 'pul' ]; then
        ml=`expr $sline + 71`        
      fi

      if [ $1 == 'grs' ]; then
        ml=`expr $sline + 67`        # for 'pasture'
      fi


      Mline=`head -n $ml $fname | tail -n 1`

      # labile organic N Mineralization
      MinSF=`echo $Mline | awk '{print $1}'`
      MinUP=`echo $Mline | awk '{print $2}'`
      MinLO=`echo $Mline | awk '{print $3}'`
      MinGW=`echo $Mline | awk '{print $4}'`
      MinTOT=`echo $Mline | awk '{print $5}'`      

      # print the output to the stdout
      echo $landseg,$landuse,$syear,$MinSF,$MinUP,$MinLO,$MinGW,$MinTOT >> $stout
  
      syear=`expr $syear + 1`
   done
done
