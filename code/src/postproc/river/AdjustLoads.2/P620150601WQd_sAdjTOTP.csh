#!/bin/csh

set SCENARIO = $argv[1]
set LOAD     = $argv[2]
set TMPDIR   = $argv[3]

mkdir ../../../../../tmp/$USER-scratch/$TMPDIR
cd    ../../../../../tmp/$USER-scratch/$TMPDIR
pwd

set RSEG   = ( SL9_2720_0001 PM7_4820_0001 JL7_6800_7070 )
set FACTOR = ( 1.0866        1.3450        2.5122        )

#set RSEG   = ( SL9_2720_0001 )
#set FACTOR = ( 0.9615        )

set I = 1
while ( $I <= ${#RSEG} )
     echo $RSEG[$I]

     if ( ! -e ../../../tmp/wdm/river/$SCENARIO/stream/$RSEG[$I].wdm.ORIG ) then
        cp -pv ../../../tmp/wdm/river/$SCENARIO/stream/$RSEG[$I].wdm ../../../tmp/wdm/river/$SCENARIO/stream/$RSEG[$I].wdm.ORIG
     endif

     if ( -e  ../../../tmp/wdm/river/$SCENARIO/stream/$RSEG[$I].wdm.ORIG ) then
        cp -v ../../../tmp/wdm/river/$SCENARIO/stream/$RSEG[$I].wdm.ORIG $RSEG[$I].wdm
        echo $SCENARIO $RSEG[$I] $LOAD $FACTOR[$I] | ../../../code/bin/AdjustLoads.exe
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

