#!/bin/csh

set SCENARIO = $argv[1]
set TMPDIR   = $argv[2]

mkdir ../../../../../tmp/$USER-scratch/$TMPDIR
cd    ../../../../../tmp/$USER-scratch/$TMPDIR
pwd

goto RUNRIVER

if ( -e problem ) then
   rm problem
endif

set RSEG      = ( SL9_2720_0001 PM7_4820_0001 JL7_7100_7030 RU5_6030_0001 JA5_7480_0001 YP4_6720_6750 YM4_6620_0003 XU3_4650_0001 EM2_3980_0001 )

set PARAMS    = ( PHYT )
set PHYT      = ( 3.45863 2.59873 4.03347 2.20594 0.41702 0.57639 0.21923 4.04837 3.54552 )

set PARAMS    = ( NH3X NO23 ORGN PO4X PIPX ORGP )
set NH3X      = ( 1.58141 1.09844 0.46382 0.62280 0.74734 0.73488 0.42031 0.89157 0.42665 )
set NO23      = ( 1.30921 1.33249 0.71811 0.67367 0.52113 0.62407 0.37193 0.97925 0.48460 )
set ORGN      = ( 0.55832 1.48415 1.65652 2.12583 2.76171 1.56592 1.36207 0.60465 2.35238 )
set PO4X      = ( 0.81098 1.31925 0.74290 1.16424 1.19291 0.79040 0.90909 1.07015 1.16342 )
set PIPX      = ( 0.64327 1.12921 11.94274 0.85938 0.17847 0.41192 0.08022 0.82943 0.12283 )
set ORGP      = ( 1.21260 2.23340 1.90743 3.12684 3.43207 2.17568 3.37825 1.31534 3.12533 )

set PARAMS    = ( PHYT )
set PHYT      = ( 1.47725 0.30169 0.31646 0.15041 0.10550 0.29346 0.21735 1.25741 0.13604 )

set I = 1
while ( $I <= ${#RSEG} )
     echo $RSEG[$I]

     set double = `grep -il $RSEG[$I] ../../../config/catalog/geo/p600/doubles.csv | wc -l`
     if ( $double == 0 ) then
        set iRSEGS = ( $RSEG[$I] )
     else
        set line = `grep $RSEG[$I] ../../../config/catalog/geo/p600/doubles.csv`
        set data = `echo $line:q | sed 's/,/ /g'`
        set iRSEGS = ( $data[2] $data[3] )
     endif

     foreach iRSEG ( $iRSEGS )
        if ( ! -e ../../../tmp/wdm/river/$SCENARIO/stream/$iRSEG.wdm.ORIG ) then
           cp -pv ../../../tmp/wdm/river/$SCENARIO/stream/$iRSEG.wdm ../../../tmp/wdm/river/$SCENARIO/stream/$iRSEG.wdm.ORIG
        endif
     end

     foreach iRSEG ( $iRSEGS )
        if ( -e  ../../../tmp/wdm/river/$SCENARIO/stream/$iRSEG.wdm ) then
           cp -v ../../../tmp/wdm/river/$SCENARIO/stream/$iRSEG.wdm $iRSEG.wdm
           foreach PARAM ( $PARAMS )
              if ( $PARAM == "PHYT" ) then
                 echo $SCENARIO $iRSEG $PARAM $PHYT[$I] | ../../../code/bin/AdjustLoads.exe
              else if ( $PARAM == "NO23" ) then
                 echo $SCENARIO $iRSEG $PARAM $NO23[$I] | ../../../code/bin/AdjustLoads.exe
              else if ( $PARAM == "NH3X" ) then
                 echo $SCENARIO $iRSEG $PARAM $NH3X[$I] | ../../../code/bin/AdjustLoads.exe
              else if ( $PARAM == "PO4X" ) then
                 echo $SCENARIO $iRSEG $PARAM $PO4X[$I] | ../../../code/bin/AdjustLoads.exe
              else if ( $PARAM == "PIPX" ) then
                 echo $SCENARIO $iRSEG $PARAM $PIPX[$I] | ../../../code/bin/AdjustLoads.exe
              else if ( $PARAM == "ORGN" ) then
                 echo $SCENARIO $iRSEG $PARAM $ORGN[$I] | ../../../code/bin/AdjustLoads.exe
              else if ( $PARAM == "ORGP" ) then
                 echo $SCENARIO $iRSEG $PARAM $ORGP[$I] | ../../../code/bin/AdjustLoads.exe
              endif
           endif
        end
        cp -v $iRSEG.wdm ../../../tmp/wdm/river/$SCENARIO/stream/$iRSEG.wdm
     endif
     end

     @ I = $I + 1
     echo $I
end

if ( -e problem ) then
   echo "ERROR PWD=`pwd`"
   cat problem
endif

exit
RUNRIVER:

cd ..
rm -r $TMPDIR

cd ../../run_bhatt/
source ../config/control/script/${SCENARIO}.con
cd $SCRIPT_DIR
csh bhatt_run_river_oneseg_calib.csh $SCENARIO JL7_7030_6800 tempRIV & wait
csh bhatt_run_river_oneseg_calib.csh $SCENARIO JL7_6800_7070 tempRIV & wait
csh bhatt_run_river_oneseg_calib.csh $SCENARIO JL7_7070_0001 tempRIV & wait
csh bhatt_run_river_oneseg_calib.csh $SCENARIO YP4_6750_0001 tempRIV & wait
csh bhatt_run_river_oneseg_calib.csh $SCENARIO YM4_6620_0001 tempRIV & wait

