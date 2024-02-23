#!/bin/csh

set RSEG = $argv[1]

   set DRSEG = $RSEG
   set DRID  = `echo "$DRSEG" | awk '{print substr($0,10,4)}'`
   set DRSEG = `grep _${DRID}_ ../../config/catalog/geo/p600/rivers.csv`
   while ( ${#DRSEG} > 0 )
      set DRID  = `echo "$DRSEG" | awk '{print substr($0,10,4)}'`
      set DRSEG = `grep _${DRID}_ ../../config/catalog/geo/p600/rivers.csv`
      echo $DRSEG
   end
