#!/bin/csh

set SCENARIO = $argv[1]
set TMPDIR   = $argv[2]


mkdir ../../../../../tmp/$USER-scratch/$TMPDIR
cd    ../../../../../tmp/$USER-scratch/$TMPDIR
pwd

set RSEG      = ( SL9_2720_0001 PM7_4820_0001 JL7_6800_7070 RU5_6030_0001 JA5_7480_0001 YP4_6720_6750 YM4_6620_0003 XU3_4650_0001 EM2_3980_0001 )
set FACTORSTN = ( 1.0000        1.2228        1.5015        1.3368        1.7701        0.9351        0.6359        1.0338        0.8545 )
set FACTORSTP = ( 1.0000        1.5581        2.1873        1.7185        1.7030        1.0925        0.7899        1.3354        0.4055 )

#set RSEG      = ( SL9_2720_0001 )
#set FACTORSTN = ( 1.0000 )
#set FACTORSTP = ( 1.0000 )

#set RSEG   = ( SL9_2720_0001 )
#set FACTOR = ( 0.9615        )

set RSEG      = ( YM4_6620_0001 )
set FACTORSTN = ( 0.6359        )
set FACTORSTP = ( 0.7899        )

set RSEG      = ( JL7_6800_7070 )
set FACTORSTN = ( 1.5015        )
set FACTORSTP = ( 2.1873        )

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
end

if ( -e problem ) then
   echo "ERROR"
   `pwd`
   cat problem
endif


cd ..
rm -r $TMPDIR




cd ../../run_bhatt/
source ../config/control/script/${SCENARIO}.con
cd $SCRIPT_DIR
#csh bhatt_run_river_oneseg_calib.csh $SCENARIO YP4_6750_0001 tempRIV
csh bhatt_run_river_oneseg_calib.csh $SCENARIO JL7_7070_0001 tempRIV




