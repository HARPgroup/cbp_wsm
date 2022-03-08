#!/bin/csh

set RIMS  = ( APPO          CHOP          JAME          MATT          PAMU          PATU          POTO          RAPP          SUSQ          )
set RSEGS = ( JA5_7480_0001 EM2_3980_0001 JL7_7100_7030 YM4_6620_0001 YP4_6720_6750 XU3_4650_0001 PM7_4820_0001 RU5_6030_0001 SL9_2720_0001 )


@ I = 1
while ( $I <= ${#RIMS} )
   printf "set segments = ( " > $RIMS[$I].riv
   foreach line ( "`tac out | grep '$RSEGS[$I],'`" )
      set data = `echo $line:q | sed 's/,/ /g'`
      printf "$data[2] " >> $RIMS[$I].riv
   end
   printf ")" >> $RIMS[$I].riv
   cp -vip $RIMS[$I].riv ../../../../config/seglists/
   @ I = $I + 1
end
