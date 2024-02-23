#!/bin/csh

foreach line1 ( "`cat cell_rseg_cbseg.csv`" )
   set data1 = `echo $line1:q | sed 's/,/ /g'`
   #cell rseg cbseg
   #echo $data1[3],$data1[2]
   foreach line2 ( "`grep $data1[2], ../../all_upstream_rsegs_with_doubles_p6.csv`" )
      set data2 = `echo $line2:q | sed 's/,/ /g'`
      #rseg upstream
      foreach line3 ( "`grep $data2[2] ../../land_water_area.csv`" )
         set data3 = `echo $line3:q | sed 's/,/ /g'`
         #rseg lseg
         echo $data1[1],$data1[2],$data1[3],$data3[2],$data3[1]
      end
   end
end
