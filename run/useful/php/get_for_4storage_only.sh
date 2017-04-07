#!/bin/sh

stout="./land/$1/$2/$4"

echo "landseg,landuse,year,Slorn1,Slorn2,Slorn3,Slorn4,Slorntot" > $stout

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
   baseyear=$syear

   # search the file for each instance of the section header NITR
   # fgrep -n send the format line_number:line_text,
   # awk, by default parses a line by spaces, but -F: asks it to 
   # parse by : (other delimiters can be used similarly)
   for i in `fgrep -n "*** NITR ***" $fname | awk -F: '{print int($1)}'`; do 
      sline=$i
      # the total line is located 14 lines from the header line
      eline1=`expr $sline + 11`
      eline2=`expr $sline + 12`
      eline3=`expr $sline + 14`
      eline4=`expr $sline + 15`
      elinetot=`expr $sline + 17`
 
      # STORAGES
      storline1=`head -n $eline1 $fname | tail -n 1`
      Slorn1=`echo $storline1 | awk '{print $7}'`
      
      storline2=`head -n $eline2 $fname | tail -n 1`
      Slorn2=`echo $storline2 | awk '{print $7}'`

      storline3=`head -n $eline3 $fname | tail -n 1`
      Slorn3=`echo $storline3 | awk '{print $7}'`

      storline4=`head -n $eline4 $fname | tail -n 1`
      Slorn4=`echo $storline4 | awk '{print $7}'`

      storline=`head -n $elinetot $fname | tail -n 1`
      Slorntot=`echo $storline | awk '{print $6}'`
 
      # print the output to the stdout
      echo $landseg,$landuse,$syear,$Slorn1,$Slorn2,$Slorn3,$Slorn4,$Slorntot >> $stout

      syear=`expr $syear + 1`
   done

done


