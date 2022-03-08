#!/bin/csh

set SCENARIO = $argv[1]
set TMPDIR   = $argv[2]

mkdir ../../../../../tmp/$USER-scratch/$TMPDIR
cd    ../../../../../tmp/$USER-scratch/$TMPDIR
pwd

#goto RUNRIVER

if ( -e problem ) then
   rm problem
endif

set RSEG      = ( SL9_2720_0001 PM7_4820_0001 JL7_7100_7030 RU5_6030_0001 JA5_7480_0001 YP4_6720_6750 YM4_6620_0003 XU3_4650_0001 EM2_3980_0001 )

set PARAMS    = ( PHYT NO23 NH3X PO4X PIPX ORGN ORGP ) 
set PHYT      = (  7.07802 17.48722 10.26905  6.12541  3.71851  3.08021  1.72697 13.98194 17.03994 )
set NO23      = (  1.06636  1.14234  0.90806  1.36481  0.85891  0.90748  1.07697  0.96682  0.63094 )
set NH3X      = (  0.97763  0.40283  0.30416  0.67960  0.78866  0.71129  0.65407  1.12977  0.60235 )
set PO4X      = (  1.50364  9.21052  1.07686  1.75297  1.30885  1.54603  1.00432  0.98236  2.53420 )
set PIPX      = (  0.38952  0.43955  0.93589  0.85416  0.15197  0.22944  0.02741  0.75374  0.15882 )
set ORGN      = ( 1.11019 3.67641 1.98419 1.98850 1.88160 1.51524 0.81796 0.91126 2.05615 )
set ORGP      = ( 0.98556 0.88883 1.31863 2.01647 2.81461 2.79382 4.11567 1.46772 1.08507 )

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
        if ( -e  ../../../tmp/wdm/river/$SCENARIO/stream/$iRSEG.wdm.ORIG ) then
           cp -v ../../../tmp/wdm/river/$SCENARIO/stream/$iRSEG.wdm.ORIG $iRSEG.wdm
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

