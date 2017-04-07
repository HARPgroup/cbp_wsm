#!/bin/csh
 source /model/p517/run/seglists/SLbig.riv

 foreach seg ( $segments )
  grep $seg SL_SLbigsgtf_sgtf.csv
 end 

