#!/bin/csh

set SCENARIO = P620160701WQe_BCSPEC1X
set SCENARIO = P620161110WQc
set SCENARIO = $argv[1]

set RSEGS    = ( SL9_2720_0001 PM7_4820_0001 JL7_7100_7030 RU5_6030_0001 JA5_7480_0001 YP4_6720_6750 YM4_6620_0003 XU3_4650_0001 EM2_3980_0001 SL9_2490_2520 )
set PARAMS   = ( NH3X ORGN ORGP PHYT PO4X PIPX TSED NO23 TOTN TOTP TSSX ) 
set PARAMS   = ( NH3X ORGN ORGP PO4X PIPX TSED NO23 TOTN TOTP TSSX )


alias MATH 'set \!:1 = `echo "\!:3-$" | bc -l`'


if ( -e ${SCENARIO}_stat.csv ) rm ${SCENARIO}_stat.csv

foreach RSEG ( $RSEGS )
   foreach PARAM ( $PARAMS )
      if ( -e ../../../../../output/river/stats/${SCENARIO}/${RSEG}_1985_2014_annual_vs_wrtds.${PARAM} ) then
         set line = "`grep ' mean          ,' ../../../../../output/river/stats/${SCENARIO}/${RSEG}_1985_2014_annual_vs_wrtds.${PARAM}`"
         set data = `echo $line:q | sed 's/,/ /g'`
      else
         set data[2] = 1
         set data[3] = 1
      endif
      set sim = `echo $data[2] | awk -F"E" 'BEGIN{OFMT="%10.10f"} {print $1 * (10 ^ $2)}'`
      set obs = `echo $data[3] | awk -F"E" 'BEGIN{OFMT="%10.10f"} {print $1 * (10 ^ $2)}'`
      MATH ratio = $obs / $sim
      #echo "$RSEG, $PARAM, `grep ' mean          ,' ../../../../../output/river/stats/${SCENARIO}/${RSEG}_1985_2014_annual_vs_wrtds.${PARAM}`" >> ${SCENARIO}_stat.csv
      echo "$RSEG, $PARAM, MEAN, $data[2], $data[3], $ratio" >> ${SCENARIO}_stat.csv
   end
end 

find ./ -name ${SCENARIO}_stat.csv | xargs perl -pi -e "s/ mean          ,/mean,/g"
