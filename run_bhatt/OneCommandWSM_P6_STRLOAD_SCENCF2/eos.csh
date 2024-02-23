#!/bin/csh

source ../../config/seglists/allP6.riv

foreach RSEG ( $segments )
   if ( ! -e ../../tmp/wdm/river/CFBASE30Y20180615/eos/${RSEG}.wdm ) then
     echo $RSEG
   endif
end

set i = 1
foreach RSEG ( $segments )
   if ( ! -e ../../tmp/wdm/river/CFBASE30Y20180615/eos/ps_sep_div_ams_CFBASE30Y20180615_${RSEG}.wdm ) then
     set RID = `echo $RSEG | awk '{print substr($0,10,13)}'`
     if ( $RID != "0000" ) then
       csh bhatt_run_etm_and_land_and_dat_simultaneously_oneseg.csh CFBASE30Y20180615 $RSEG 1985 2014 tempETM_$i &
       #echo $RSEG
       @ i = $i + 1
     endif
   endif
end
