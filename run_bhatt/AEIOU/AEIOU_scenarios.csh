#!/bin/csh

set LINENO = 4
set LINENO = 1
set PARAMS = ( NH3X NO23 RORN BODN PO4X RORP BODP )
set RSEGS  = ( SU2_0030_0140 SU7_0850_0730 SL8_1760_1780 SL9_2270_2380 PS2_6730_6660 PS4_6360_5840 JU1_6300_6650 JU5_7500_7420 )
set LOAD   = 10000

foreach RSEG ( $RSEGS )
   foreach PARAM ( $PARAMS )
      @ XLOAD = $LOAD
      if ( $PARAM == "PO4X" || $PARAM == "RORP" || $PARAM == "BODP" ) then
         @ XLOAD = $LOAD / 10
      endif
      printf '%02d,%s,%s,%05d\n' $LINENO $PARAM $RSEG $XLOAD
      @ LINENO = $LINENO + 1
   end
end
