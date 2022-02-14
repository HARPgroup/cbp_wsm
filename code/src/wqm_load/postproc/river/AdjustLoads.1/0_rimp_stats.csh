#!/bin/csh

set RSEGS      = ( SL9_2720_0001 PM7_4820_0001 JL7_7100_7030 RU5_6030_0001 JA5_7480_0001 YP4_6720_6750 YM4_6620_0003 XU3_4650_0001 EM2_3980_0001 )


cd ../../../../../run/calibration/bhatt_OTHER

foreach RSEG ( $RSEGS )
   sbatch step_run_river_stats.csh P620161110WQd_BCSPEC1X $RSEG `uuidgen`
end
