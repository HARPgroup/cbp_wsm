#!/bin/sh

anout="./land/$3/$4/$6"
#meanout="./land/$3/$4/$7"

echo "prj,try,scenario,landseg,landuse,year,Snh4,Sno3,Sslorn,Sdlorn,Ssrorn,Sdrorn,Splant,FdinQout,FdinGout,FlornQout,FlornGout,FrornQout,FrornGout,Tbgret,Tup,Timm,Tdenit,Tvol,Tl2r" > $anout
#echo "projectid,scenarioid,model_scen,subshedid,landseg,luname,thisyear,constit,annual_cum,annual_diff,annual_uptake,annual_eof" > $anout

#echo "landseg,constit,mean_uptk" > $meanout

# $1 = projectid default:65
# $2 = scenarioid default:1
# $3 = landuse
# $4 = scenario name (such as nut and newnut)
# $5 = land segment ( or 'all' for all)
# $6 = output file name
# $7 summary output file

if [ $5 == 'all' ]; then
   files="../output/hspf/land/out/$3/$4/*.out"
else
   files="../output/hspf/land/out/$3/$4/$3$5.out"
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
   landuse=$3
   landseg=`echo $j | awk -F/ '{print(substr($5, 4, 6))}'`
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
   # search the file for each instance of the section header NITR
   # fgrep -n send the format line_number:line_text,
   # awk, by default parses a line by spaces, but -F: asks it to 
   # parse by : (other delimiters can be used similarly)
   for i in `fgrep -n "*** NITR ***" $fname | awk -F: '{print int($1)}'`; do 
      sline=$i
      # the total line is located 14 lines from the header line
      # there's gotta be a better way, but, pick out the desired line
      # by grabbing all lines up to and including the desired line
      # then sending it to tail, and grabbing the last line (the one
      #   we really wanted)

      # FLUXES
      FlornNo=`expr $sline + 36`
      FrornNo=`expr $sline + 37`

      Flornline=`head -n $FlornNo $fname | tail -n 1`
      Frornline=`head -n $FrornNo $fname | tail -n 1`

      FlornSout=`echo $Flornline | awk '{print $3}'`
      FlornUout=`echo $Flornline | awk '{print $7}'`
      FlornGout=`echo $Flornline | awk '{print $10}'`

      FrornSout=`echo $Frornline | awk '{print $3}'`
      FrornUout=`echo $Frornline | awk '{print $7}'`
      FrornGout=`echo $Frornline | awk '{print $10}'`

      # calculate quickflow fluxes as surface + upper
      FlornQout=`echo "$FlornSout $FlornUout" | awk '{print($1 + $2)}'`
      FrornQout=`echo "$FrornSout $FrornUout" | awk '{print($1 + $2)}'`
      
      # for some reasons, expr didn't like the storline and antotal
      # so I had to use awk to subtract them
      # andiff=`echo "$antotal $lasttotal" | awk '{print($1 - $2)}'`

      # print the output to the stdout
      echo $prj,$scn,$4,$landseg,$landuse,$syear,$FlornQout,$FlornGout,$FrornQout,$FrornGout >> $anout
#      if [ $syear > $baseyear ];then
#         cumulativen=`echo "$cumulativen  $anuptk" | awk '{print($1 + $2)}'`
#         numyears=`expr $numyears + 1`
#      fi
     # echo "$cumulativen + $anuptk"
      syear=`expr $syear + 1`

      # stash the current year total for use next loop
#      lasttotal=$antotal
   done
#   avgn=`echo "$cumulativen $numyears" | awk '{print($1 / $2)}'`
#   echo "$landseg,totn,$avgn" >> $meanout
done


