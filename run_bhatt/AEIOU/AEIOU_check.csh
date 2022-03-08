#!/bin/csh


set PARAMS = ( NH3X NO23 RORN BODN PO4X RORP BODP )
set RSEGS  = ( SU2_0030_0140 SU7_0850_0730 SL8_1760_1780 SL9_2270_2380 PS2_6730_6660 PS4_6360_5840 JU1_6300_6650 JU5_7500_7420 )
set LOAD   = 10000


foreach RSEG ( $RSEGS )
   set RID = `echo $RSEG | awk '{print substr($0,10,4)}'`
      if ( $RID != "0000") then
         foreach PARAM ( $PARAMS )
            set line = "`grep $RSEG AEIOU_scenarios | grep $PARAM`"
            set data = `echo $line:q | sed 's/,/ /g'`
            set LOAD = $data[4]
            set FILENAME = /modeling/gb604/output/river/annual/20190212_AEIOU/AVGANN_${RSEG}_${PARAM}_${LOAD}.csv
            if ( -e $FILENAME ) then
               #echo "... ...`wc -l $FILENAME`"
            else
               echo "... error ${RSEG} ${PARAM} ${LOAD} :: `grep $RSEG AEIOU_scenarios | grep ${PARAM}`"
            endif
         end
      endif
end

