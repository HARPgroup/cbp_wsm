#!/bin/csh

source ../../config/seglists/allP6.riv

foreach RSEG ( $segments )

   set DRSEG = $RSEG
   set DRID  = `echo "$DRSEG" | awk '{print substr($0,10,4)}'`
   set DRSEG = `grep _${DRID}_ ../../config/catalog/geo/p600/rivers.csv`
   while ( ${#DRSEG} > 0 )
      #set DRID  = `echo "$DRSEG" | awk '{print substr($0,10,4)}'`
      #set DRSEG = `grep _${DRID}_ ../../config/catalog/geo/p600/rivers.csv`
      echo $RSEG $DRSEG
      set DRID  = `echo "$DRSEG" | awk '{print substr($0,10,4)}'`
      set DRSEG = `grep _${DRID}_ ../../config/catalog/geo/p600/rivers.csv`
   end
end

