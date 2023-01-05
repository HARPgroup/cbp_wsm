#!/bin/csh

# csh nonRIM.csh > ../p600_JoelRIM01_lrsegs.csv

source ../../../../../seglists/watershed.riv

echo "cell,lseg,rseg,weight***"
foreach RSEG ( $segments )
   set COUNT = `grep -l $RSEG ../../../../../seglists/RIMP.riv | wc -l`
   if ( $COUNT == 0 ) then
      #echo $RSEG
      foreach line ( "`grep $RSEG ../../land_water_area.csv`" )
         set data = `echo $line:q | sed 's/,/ /g'`
         echo "xRIM,$data[2],$data[1],1.000000"

         set RID = `echo $data[1] | awk '{print substr($0,1,1)}'`
         set LID = `echo $data[2] | awk '{print substr($0,2,2)}'`

         if ( $RID == 'E' || $RID == 'S' ) then
            echo "xRES,$data[2],$data[1],1.000000"
         else 
            echo "xRWS,$data[2],$data[1],1.000000"
         else

         endif
      end
   endif
end
