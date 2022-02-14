#!/bin/csh

set MYBIN    = AdjustLoads2

set OUTLOG   = 1

set SCENARIO = $argv[1]
set TMPDIR   = $argv[2]

#goto RUNRIVER

mkdir ../../../../../tmp/$USER-scratch/$TMPDIR
cd    ../../../../../tmp/$USER-scratch/$TMPDIR
pwd

set RSEGS      = ( SL9_2720_0001 PM7_4820_0001 JL7_7100_7030 RU5_6030_0001 JA5_7480_0001 YP4_6720_6750 YM3_6430_6620 YM1_6370_6620 XU3_4650_0001 EM2_3980_0001 )
set FACTORSFL  = ( 1.0000        1.0000        1.0000        1.0000        1.0000        1.0000        1.0000        1.0000        1.0000        1.0000 )
set FACTORSTN  = ( 0.9717        1.1317        1.0632        1.2810        0.9617        1.0434        0.8831        0.8831        0.6529        0.5664 )
set FACTORSTP  = ( 0.9845        0.9660        1.3619        0.8553        0.6913        0.6492        0.6081        0.6081        0.8961        0.3985 )
set FACTORSSS  = ( 1.0000        1.0000        1.0000        1.0000        1.0000        1.0000        1.0000        1.0000        1.0000        1.0000 )

#set RSEGS      = ( PM7_4820_0001 )
#set FACTORSFL  = ( 1.0000 )
#set FACTORSTN  = ( 1.1317 )
#set FACTORSTP  = ( 0.9660 )
#set FACTORSSS  = ( 1.0000 )

set RSEGS      = ( YM3_6430_6620 YM1_6370_6620 )
set FACTORSFL  = ( 1.0000        1.0000 )
set FACTORSTN  = ( 0.8831        0.8831 )
set FACTORSTP  = ( 0.6081        0.6081 )
set FACTORSSS  = ( 1.0000        1.0000 )


set I = 1
while ( $I <= ${#RSEGS} )
     echo $RSEGS[$I]

     if ( ! -e ../../../tmp/wdm/river/$SCENARIO/stream/$RSEGS[$I].wdm.ORIG ) then
        cp -vip ../../../tmp/wdm/river/$SCENARIO/stream/$RSEGS[$I].wdm ../../../tmp/wdm/river/$SCENARIO/stream/$RSEGS[$I].wdm.ORIG
     endif

     if ( -e  ../../../tmp/wdm/river/$SCENARIO/stream/$RSEGS[$I].wdm.ORIG ) then
        cp -v ../../../tmp/wdm/river/$SCENARIO/stream/$RSEGS[$I].wdm.ORIG $RSEGS[$I].wdm
        echo $SCENARIO $RSEGS[$I] $OUTLOG $FACTORSFL[$I] $FACTORSTN[$I] $FACTORSTP[$I] $FACTORSSS[$I]| ../../../code/bin/${MYBIN}.exe
        cp -v $RSEGS[$I].wdm ../../../tmp/wdm/river/$SCENARIO/stream/$RSEGS[$I].wdm
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

exit

RUNRIVER:

cd ../../../../../run_bhatt/
source ../config/control/script/${SCENARIO}.con
cd $SCRIPT_DIR
csh bhatt_run_river_oneseg_calib.csh $SCENARIO JL7_7030_6800 tempRIV & wait
csh bhatt_run_river_oneseg_calib.csh $SCENARIO JL7_6800_7070 tempRIV & wait
csh bhatt_run_river_oneseg_calib.csh $SCENARIO JL7_7070_0001 tempRIV & wait
csh bhatt_run_river_oneseg_calib.csh $SCENARIO YP4_6750_0001 tempRIV & wait
csh bhatt_run_river_oneseg_calib.csh $SCENARIO YM4_6620_0001 tempRIV & wait




