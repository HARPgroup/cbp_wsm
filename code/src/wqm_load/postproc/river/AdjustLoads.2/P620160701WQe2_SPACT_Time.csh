#!/bin/csh

# csh P620160701WQe2_SPACT_Time.csh P620160701WQe2 tempADJ1 > P620160701WQe2_SPACT_Time.out

set MYBIN    = AdjustLoads2

set OUTLOG   = 1

set SCENARIO = $argv[1]
set TMPDIR   = $argv[2]

#goto RUNRIVER

mkdir ../../../../../tmp/$USER-scratch/$TMPDIR
cd    ../../../../../tmp/$USER-scratch/$TMPDIR
set MYPWD = `pwd`

set RSEGS      = ( SL9_2720_0001 PM7_4820_0001 JL7_7100_7030 RU5_6030_0001 JA5_7480_0001 YP4_6720_6750 YM3_6430_6620 YM1_6370_6620 XU3_4650_0001 EM2_3980_0001 )
set FACTORSFL  = ( 1.0000        1.0000        1.0000        1.0000        1.0000        1.0000        1.0000        1.0000        1.0000        1.0000 )
set FACTORSTN  = ( 1.1552        1.5529        1.5170        1.6345        1.3872        1.2282        0.8599        0.8599        1.0038        0.8395 )
set FACTORSTP  = ( 1.0000        1.1620        1.3046        1.2786        0.9579        0.8592        0.8847        0.8847        1.0802        0.7572 )
set FACTORSSS  = ( 1.0000        1.0000        1.0000        1.0000        1.0000        1.0000        1.0000        1.0000        1.0000        1.0000 )


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

     cd $MYPWD

end

#if ( -e problem ) then
#   echo "ERROR"
#   `pwd`
#   cat problem
#endif


cd ..
rm -r $TMPDIR

#exit

RUNRIVER:

cd ../../../../../run_bhatt/
source ../config/control/script/${SCENARIO}.con
cd $SCRIPT_DIR
csh bhatt_run_river_oneseg_calib.csh $SCENARIO JL7_7030_6800 tempRIV & wait
csh bhatt_run_river_oneseg_calib.csh $SCENARIO JL7_6800_7070 tempRIV & wait
csh bhatt_run_river_oneseg_calib.csh $SCENARIO JL7_7070_0001 tempRIV & wait
csh bhatt_run_river_oneseg_calib.csh $SCENARIO YP4_6750_0001 tempRIV & wait
csh bhatt_run_river_oneseg_calib.csh $SCENARIO YM4_6620_0001 tempRIV & wait




