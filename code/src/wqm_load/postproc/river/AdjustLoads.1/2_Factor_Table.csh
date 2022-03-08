#!/bin/csh

set OUTPUT    = "F" #factors
#set OUTPUT    = "P" #percent

set SCENARIO  = P620160701WQe_BCSPEC1X
set SCENARIO  = P620161110WQc
set SCENARIO = $argv[1]

set RSEGS     = ( SL9_2720_0001 PM7_4820_0001 JL7_7100_7030 RU5_6030_0001 JA5_7480_0001 YP4_6720_6750 YM4_6620_0003 XU3_4650_0001 EM2_3980_0001 )
set RNAMES    = ( SUSQ POTO JAME RAPP APPO PAMU MATT PATU CHOP )
set PARAMS    = ( PHYT NO23 NH3X PO4X PIPX ORGN ORGP TOTN TOTP )
set PARAMS    = ( NH3X NO23 ORGN TOTN PO4X PIPX ORGP TOTP TSED TSSX PHYT )


alias MATH 'set \!:1 = `echo "\!:3-$" | bc -l`'

echo "                    $RNAMES"
foreach PARAM ( $PARAMS )
   printf "set %s      = (" "$PARAM"
   foreach RSEG ( $RSEGS )
      set line = `grep "$RSEG, $PARAM" ${SCENARIO}_stat.csv`
      set data = `echo $line:q | sed 's/,/ /g'`
      if ( $OUTPUT == 'F' ) then
         printf " %3.5f" "$data[6]"
      else
         MATH pct = (  1/$data[6] - 1 ) * 100
         printf "\t%2.1f" "$pct"
      endif
   end
   printf " )\n"
end
