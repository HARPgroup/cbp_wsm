#!/bin/csh

set SCENARIO = $argv[1]
set TMPDIR   = $argv[2]


mkdir ../../../../../tmp/$USER-scratch/$TMPDIR
cd    ../../../../../tmp/$USER-scratch/$TMPDIR
pwd

set RSEG      = ( SL9_2720_0001 PM7_4820_0001 JL7_7100_7030 RU5_6030_0001 JA5_7480_0001 YP4_6720_6750 YM4_6620_0003 XU3_4650_0001 EM2_3980_0001 )
set FACTORSTN = ( 0.9717        1.1317        1.0632        1.2810        0.9617        1.0434        0.8831        0.6529        0.5664 )
set FACTORSTP = ( 0.9845        0.9660        1.3619        0.8553        0.6913        0.6492        0.6081        0.8961        0.3985 )

set I = 1
while ( $I <= ${#RSEG} )
     echo $RSEG[$I]

     if ( ! -e ../../../tmp/wdm/river/$SCENARIO/stream/$RSEG[$I].wdm.ORIG ) then
        cp -pv ../../../tmp/wdm/river/$SCENARIO/stream/$RSEG[$I].wdm ../../../tmp/wdm/river/$SCENARIO/stream/$RSEG[$I].wdm.ORIG
     endif

     if ( -e  ../../../tmp/wdm/river/$SCENARIO/stream/$RSEG[$I].wdm.ORIG ) then
        cp -v ../../../tmp/wdm/river/$SCENARIO/stream/$RSEG[$I].wdm.ORIG $RSEG[$I].wdm
        echo $SCENARIO $RSEG[$I] TOTN $FACTORSTN[$I] | ../../../code/bin/AdjustLoads.exe
        echo $SCENARIO $RSEG[$I] TOTP $FACTORSTP[$I] | ../../../code/bin/AdjustLoads.exe
        cp -v $RSEG[$I].wdm ../../../tmp/wdm/river/$SCENARIO/stream/$RSEG[$I].wdm
        echo "cp"
     endif

     @ I = $I + 1
     echo $I

     if ( -e problem ) then
        echo "ERROR"
        `pwd`
        cat problem
        exit
     endif

end

#if ( -e problem ) then
#   echo "ERROR"
#   `pwd`
#   cat problem
#endif


cd ..
rm -r $TMPDIR




cd ../../run_bhatt/
source ../config/control/script/${SCENARIO}.con
cd $SCRIPT_DIR
csh bhatt_run_river_oneseg_calib.csh $SCENARIO JL7_7030_6800 tempRIV & wait
csh bhatt_run_river_oneseg_calib.csh $SCENARIO JL7_6800_7070 tempRIV & wait
csh bhatt_run_river_oneseg_calib.csh $SCENARIO JL7_7070_0001 tempRIV & wait
csh bhatt_run_river_oneseg_calib.csh $SCENARIO YP4_6750_0001 tempRIV & wait





