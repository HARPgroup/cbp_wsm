#!/bin/csh

set SCENARIO = P620160401WQb_AdjLoads

set RSEGS      = ( SL9_2720_0001 PM7_4820_0001 JL7_7100_7030 RU5_6030_0001 JA5_7480_0001 YP4_6720_6750 YM4_6620_0001 XU3_4650_0001 EM2_3980_0001 )

cd ../../../../../run/calibration/bhatt_OTHER/

foreach RSEG ( $RSEGS )

   csh bhatt_run_river_stats.csh $SCENARIO $RSEG 1985 2014 temp$RSEG

end
